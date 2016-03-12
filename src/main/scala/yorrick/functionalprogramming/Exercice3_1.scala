package yorrick.functionalprogramming

import scala.annotation.tailrec


object Exercice3_1 {
  
  trait Animal
  case class Mamal(name: String) extends Animal
  
  // covariance: a producer of type B is a producer of type A if B is a A (inherits A)
  val funcCov: String => Animal = _ => Mamal("zebra")
  
  // contravariance: a consumer of type A is a consumer of type B if B is a A (inherits A)
  val funcCont: Mamal => Int = (a: Animal) => 1
  
  // Function1 is both co and contra variant
  val func: Mamal => Animal = (a: Animal) => Mamal("zebra")

  def main(args: Array[String]): Unit = {
  }
}
