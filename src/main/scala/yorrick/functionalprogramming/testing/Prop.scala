package yorrick.functionalprogramming.testing

import java.util.concurrent.{ExecutorService, Executors}

import Prop._
import Gen._
import SGen._
import yorrick.functionalprogramming.Par
import yorrick.functionalprogramming.testing.CompleteStream.IterableLike


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
  def zip[B](g: Gen[B]): Gen[(A, B)] = map2(g)((_, _))

  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)
  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(s => this.listOfN(s))
  // takes values from both gens with equal likelihood

  def unsized: SGen[A] = SGen(_ => this)
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
  def forAll[A](g: Int => Gen[A])(p: A => Boolean): Prop = Prop { (maxSize, testCasesNb, rng) =>
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

    val ES: ExecutorService = Executors.newCachedThreadPool
    val p1 = Prop.forAll(Gen.unit(Par.unit(1))) { i =>
      Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get
    }
    
    run(check(true))

    val maxByteProp = checkAll[Byte](_.toInt < 127)

    run(maxByteProp)
  }
}