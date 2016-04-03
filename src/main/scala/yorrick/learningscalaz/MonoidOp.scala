package yorrick.learningscalaz

import scala.language.implicitConversions


// type class, enrich all classes that have an instance for monoid
trait MonoidOp[A] {
  val F: Monoid[A]
  val value: A
  def |+|(a2: A) = F.mappend(value, a2)
}

object MonoidOp {
  implicit def toMonoidOp[A: Monoid](a: A): MonoidOp[A] = new MonoidOp[A] {
    val F = implicitly[Monoid[A]]
    val value = a
  }
}

object TestMonoidOp {
  def main (args: Array[String] ) {
    import MonoidOp._
    
    // int and string monoids come from Monoid type class
    println(3 |+| 4)
    println("fvkp" |+| "kfl")
  }
}