package yorrick.learningscalaz

import scala.language.implicitConversions
import Plus._

trait Plus[A] {
  def plus(a1: A, a2: A): A
}

object Plus {
  // context bound
  def plus[A: Plus](a1: A, a2: A): A = implicitly[Plus[A]].plus(a1, a2)
}

object PlusTest {
  def main (args: Array[String]) {
    implicit val intPlus: Plus[Int] = new Plus[Int] {
      def plus(i1: Int, i2: Int): Int = i1 + i2
    } 
    
    println(plus(1, 2))
  }
}
