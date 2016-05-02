package yorrick.functionalprogramming.testing

import java.util.concurrent.{ExecutorService, Executors}

import Prop._
import Gen._
import SGen._
import yorrick.functionalprogramming.Par
import yorrick.functionalprogramming.Par
import yorrick.functionalprogramming.Par.Par
import yorrick.functionalprogramming.testing.CompleteStream.IterableLike
import Math._
import yorrick.functionalprogramming.testing._

import scala.util.Try


case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)
  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen(size => apply(size).flatMap(a => f(a)(size)))
  def map[B](f: A => B): SGen[B] = flatMap(a => SGen.unit(f(a)))
  def zip[B](s2: SGen[B]): SGen[(A,B)] = SGen(size => (apply(size).zip(s2(size))))
}

object SGen {
  def unit[A](a: => A): SGen[A] = SGen(_ => Gen.unit(a))
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(size => g.listOfN(size))
  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n max 1))
}


// wraps a state transition
case class Gen[+A](sample: State[RNG, A]) {
  // primitive operation
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))
  def foldRight[B](z: B)(f: (A, B) => B): State[RNG, B] = map(a => f(a, z)).sample

  // ?
//  def filter(f: A => Boolean): Gen[A] = Gen(sample.)
  // fold? reduce?


  // derived operations
  def map[B](f: A => B): Gen[B] = flatMap(a => Gen.unit(f(a)))
  def map2[B, C](gen: Gen[B])(f: (A, B) => C): Gen[C] = flatMap(a => gen.map(b => f(a, b)))
  def **[B](g: Gen[B]): Gen[(A, B)] = map2(g)((_, _))
  def zip[B](g: Gen[B]): Gen[(A, B)] = map2(g)((_, _))

  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)
  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(s => this.listOfN(s))
  // takes values from both gens with equal likelihood

  def unsized: SGen[A] = SGen(_ => this)
}


object ** {
  def unapply[A,B](p: (A,B)) = Some(p)
}


object Gen {
  def flatten[A](g: Gen[Gen[A]]): Gen[A] = g.flatMap(identity)

  // primitive
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
  def sequence[A](gens: List[Gen[A]]): Gen[List[A]] = Gen(State.sequence(gens.map(_.sample)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt)).map(n => start + n % (stopExclusive - start))

  def choose2(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
    Gen(State(RNG.nonNegativeInt)).zip(Gen(State(RNG.nonNegativeInt)))

  def fromInt[A](f: Int => A): Gen[A] = Gen(State(RNG.nonNegativeInt)).map(f)

  def toOpt[A](g: Gen[A]): Gen[Option[A]] = g.map(Some(_))

  def boolean: Gen[Boolean] = Gen(State(RNG.nonNegativeInt)).map(n => n % 2 == 0)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = sequence(List.fill(n)(g))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = Gen.boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Double, Gen[A]), g2: (Double, Gen[A])): Gen[A] = {
    val g1Threshold = g1._1.abs / (g1._1.abs + g2._1.abs)

    Gen(State(RNG.double)).flatMap { d =>
      if (d <= g1Threshold) g1._2
      else g2._2
    }
  }

  def genStringIntFn(g: Gen[Int]): Gen[String => Int] = g.map(i => string => Try(string.toInt).getOrElse(-1) + i)

  def genStringFn[A](g: Gen[A]): Gen[String => A] = Gen {
    State { (rng: RNG) =>
      val (seed, rng2) = rng.nextInt // we still use `rng` to produce a seed, so we get a new function each time
      
      // this function is always different since it uses rng2, as said above
      // the state is also changed depending on the value of the string, so resulting A will depend on rng AND string value
      val f: String => A = (s: String) => g.sample.run(RNG.Simple(seed.toLong ^ s.hashCode.toLong))._1
      
      // return the function f and rng2
      (f, rng2)
    }
  }

  def genFn[I, O](g: Gen[O]): Gen[I => O] = Gen {
    State { (rng: RNG) =>
      val (seed, rng2) = rng.nextInt // we still use `rng` to produce a seed, so we get a new function each time

      // this function is always different since it uses rng2, as said above
      // the state is also changed depending on the value of the string, so resulting A will depend on rng AND string value
      val f: I => O = (s: I) => g.sample.run(RNG.Simple(seed.toLong ^ s.hashCode.toLong))._1

      // return the function f and rng2
      (f, rng2)
    }
  }
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop { (ms, testCases, rng) =>
    run(ms, testCases, rng) match {
      case Passed => p.run(ms, testCases, rng)
      case Proved => p.run(ms, testCases, rng)
      case x => x
    }
  }

  def ||(p: Prop): Prop = Prop { (ms, testCases, rng) =>
    run(ms, testCases, rng) match {
      case f: Falsified => p.run(ms, testCases, rng)
      case x => x
    }
  }

}

object Prop {
  type TestCases = Int  // number of test cases
  type SuccessCount = Int  // number of successes
  type FailedCase = String  // failure message
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case object Proved extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  // build a property with a sized generator, forwards call to forAll(g: Int => Gen)
  def forAll[A](g: SGen[A])(p: A => Boolean): Prop = forAll(x => g(x))(p)

  // utility function
  private def forAll[A](g: Int => Gen[A])(p: A => Boolean): Prop = Prop { (maxSize, testCasesNb, rng) =>
    // for each size, generate this number of random cases
    val casesPerSize = (testCasesNb + (maxSize - 1)) / maxSize  // nbTestCases = 2, max = 4, casesPerSize = 1

    // make one property per size, but no more than nbTestCases properties
    val props: Stream[Prop] = Stream.from(0).take(testCasesNb.min(maxSize) + 1).map(i => forAll(g(i))(p))

    // combine all props into one
    val prop: Prop = props.map(p => Prop { (max, _, rng) =>
      p.run(max, casesPerSize, rng)
    }).toList.reduce(_ && _)

    prop.run(maxSize, testCasesNb, rng)
  }

  // Define a new Prop from a Generator, and a predicate. Define run method of Prop.
  // Test code is contained in predicate p
  // Ignores maxSize
  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (_, testCasesNb, rng) =>
    // uses stream API
    randomStream(as)(rng).zip(Stream.from(0)).take(testCasesNb).map { case (a, i) =>
      try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }

  def checkAll[A: Ordering](f: A => Boolean)(implicit ev: IterableLike[A]): Prop = Prop { (_, _, _) =>
    CompleteStream.iterableStream[A].zip(Stream.from(0)).map { case (a, i) =>
      try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Proved)
  }

  val S = weighted(
    .75 -> choose(1, 4).map(Executors.newFixedThreadPool),
    .25 -> Gen.unit(Executors.newCachedThreadPool)
  )

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S.map2(g)((_,_))) { case (s,a) => f(a)(s).get }

  def forAllPar[A](g: SGen[A])(f: A => Par[Boolean]): Prop =
    forAll(x => S.map2(g(x))((_,_))) { case (s,a) => f(a)(s).get }

  // define an infinite stream by repeatedly sampling from given generator
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  // builds message from tested value and exception
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def run(p: Prop, maxSize: MaxSize = 100, testCases: TestCases = 100, rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  }

  /**
   * Builds nested Par[Int] pars
   */
  val pint: Gen[Par[Int]] = Gen.choose(0,10) flatMap { smallInt: Int =>
    Gen.choose(0, 2).flatMap { choice => choice match {
      case 0 => pint  // recursive call, continue nesting Par
      case 1 => Gen.unit(Par.fork(Par.unit(smallInt)))  // fork
      case 2 => Gen.unit(Par.unit(smallInt))  // simple unit
      case 3 => Gen.unit(Par.map2(Par.unit(smallInt), Par.unit(1))(_ + _))  // map2
    }}
  }
}


object CompleteStream {

  trait IterableLike[A] {
    def min: A
    def max: A
    def increment(a: A): A
  }

  object IterableLike {
    implicit object ByteCompleteStreamLike extends IterableLike[Byte] {
      def min = Byte.MinValue
      def max = Byte.MaxValue
      def increment(b: Byte): Byte = (b + 1).toByte
    }
  }

  trait EnumLike[A] {
    def values: Seq[A]
  }

  object EnumLike {
    implicit object BooleanEnumLike extends EnumLike[Boolean] {
      val values = Seq(true, false)
    }
  }

  def iterableStream[A: Ordering](implicit ev: IterableLike[A]) = {
    def go(a: A): Stream[A] =
      if (implicitly[Ordering[A]].compare(a, ev.max) < 0) {
        Stream.cons(a, go(ev.increment(a)))
      } else Stream.empty

    go(ev.min)
  }

  def enumStream[A: EnumLike] = Stream(implicitly[EnumLike[A]].values: _*)

  def main(args: Array[String]) {
    println(iterableStream[Byte].take(10).toList)
    println(enumStream[Boolean].take(10).toList)
  }

}


object Test {
  def main(args: Array[String]) {
    val smallInt = Gen.choose(-10, 10)

    val maxProp = forAll(listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
    run(maxProp)

    val sortedProp = forAll(listOf(smallInt)) { ns =>
      val sorted = ns.sorted
      sorted.isEmpty || sorted.tail.isEmpty || !ns.zip(Seq.fill(ns.length)(sorted.last)).exists { case (a, max) => a > max }
    }
    run(sortedProp)

    val maxByteProp = checkAll[Byte](_.toInt < 127)
    run(maxByteProp)

//    val parProp = Prop.checkPar {
//      Par.equal (
//        Par.map(Par.unit(1))(_ + 1),
//        Par.unit(2)
//      )
//    }
//    run(parProp)

//    println("mapParProp")
//    val mapParProp = Prop.forAllPar(pint) { intPar: Par[Int] =>
//        Par.equal (
//          Par.map(intPar)(identity),
//          intPar
//        )
//    }
//    run(mapParProp)

//    Express the property about fork from chapter 7, that fork(x) == x.
//    println("forkProp")
//    val forkProp = Prop.forAllPar(pint) { intPar: Par[Int] =>
//      Par.equal(
//        Par.fork(intPar),
//        intPar
//      )
//    }
    // commented because this version of Par dead locks
//    run(forkProp, testCases = 1)


//    We want to enforce that `takeWhile` returns the _longest_ prefix whose elements satisfy the predicate. There are various ways to state this, but the general idea is that the remaining list, if non-empty, should start with an element that does _not_ satisfy the predicate.

    
    println("takeWhileProp")
    val isEven = (i: Int) => i % 2 == 0
    val takeWhileProp = Prop.forAll(listOf(smallInt)) { intList =>
      intList.takeWhile(isEven).forall(isEven)
    }
    run(takeWhileProp)

    println("takeWhileProp2")
    val takeWhileProp2 = Prop.forAll(listOf(smallInt ** Gen.genStringIntFn(smallInt))) { list: List[(Int, String => Int)] =>
      val (intList, functionList) = list.unzip

      functionList.forall { f =>
        val fn: String => Boolean = f andThen isEven
        intList.map(_.toString).takeWhile(fn).forall(fn)
      }
    }
    run(takeWhileProp2)
    
    println("takeWhileProp3")
    val takeWhileProp3 = Prop.forAll(listOf(smallInt ** Gen.genStringFn(smallInt))) { list: List[(Int, String => Int)] =>
      val (intList, functionList) = list.unzip

      functionList.forall { f =>
        val fn: String => Boolean = f andThen isEven
        intList.map(_.toString).takeWhile(fn).forall(fn)
      }
    }
    run(takeWhileProp3)
    
    println("dropProp")
    val dropProp = Prop.forAll(listOf(smallInt ** Gen.choose(0, 1000))) { list =>
      val (intList, dropNList) = list.unzip

      dropNList.forall { d =>
        val drop = d min intList.size
        intList.drop(drop).size + drop == intList.size
      }
    }
    run(dropProp)

    val filterGen: Gen[Int => Boolean] = genFn[Int, Int](smallInt).map(f => f andThen(_ % 2 == 0))

    println("filterProp")
    val filterProp = Prop.forAll(listOf(smallInt ** filterGen)) { list: List[(Int, Int => Boolean)] =>
      val (intList, functionList) = list.unzip

      functionList.forall { f =>
        intList.filter(f).forall(f)
      }
    }
    run(filterProp)


    println("unfoldProp")

    val s = Stream.unfold(List(1,2,4)){
      case Nil => None
      case head :: t => Some((head, t))
    }
  }
}
