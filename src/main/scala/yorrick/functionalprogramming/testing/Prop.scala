package yorrick.functionalprogramming.testing

import Prop._
import Gen._


// wraps a state transition
case class Gen[A](sample: State[RNG, A]) {
  // primitive operation
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))
  def foldRight[B](z: B)(f: (A, B) => B): State[RNG, B] = map(a => f(a, z)).sample

  // ?
//  def filter(f: A => Boolean): Gen[A] = Gen(sample.)
  // fold? reduce?


  // derived operations
  def map[B](f: A => B): Gen[B] = flatMap(a => unit(f(a)))
  def map2[B, C](gen: Gen[B])(f: (A, B) => C): Gen[C] = flatMap(a => gen.map(b => f(a, b)))
  def zip[B](g: Gen[B]): Gen[(A, B)] = map2(g)((_, _))

  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)
  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(s => this.listOfN(s))
  
  // takes values from both gens with equal likelihood
  def union(g: Gen[A]): Gen[A] = Gen.boolean.flatMap(b => if (b) this else g)
}


object Gen {
  def main (args: Array[String]) {
    println(choose(2, 5))
  }
  
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
  
  def weighted[A](g1: (Double, Gen[A]), g2: (Double, Gen[A])): Gen[A] = {
    val g1Threshold = g1._1.abs / (g1._1.abs + g2._1.abs)
    
    Gen(State(RNG.double)).flatMap { d =>
      if (d <= g1Threshold) g1._2
      else g2._2
    }
  }
  
}


trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
//  def &&(p: Prop): Prop = new Prop { val check = check && p.check}
}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
  
//  def forAll[A](g: Gen[A])(p: A => Boolean): Prop
}