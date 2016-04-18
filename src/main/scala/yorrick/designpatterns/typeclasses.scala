package yorrick.designpatterns

import yorrick.functionalprogramming.testing.Stream

import annotation.implicitNotFound


object Math {
  
  @implicitNotFound("No member of type class NumberLike in scope for ${T}")
  trait NumberLike[T] {
    def plus(x: T, y: T): T
    def divide(x: T, y: Int): T
    def minus(x: T, y: T): T
  }
  
  object NumberLike {
    implicit object NumberLikeDouble extends NumberLike[Double] {
      def plus(x: Double, y: Double): Double = x + y
      def divide(x: Double, y: Int): Double = x / y
      def minus(x: Double, y: Double): Double = x - y
    }
  }
}


object Statistics {
  import Math.NumberLike
  
  def mean[T](xs: Vector[T])(implicit ev: NumberLike[T]): T =
    ev.divide(xs.reduce(ev.plus(_, _)), xs.size)
  
  def mean2[T: NumberLike](xs: Vector[T]) = {
    val ev = implicitly[NumberLike[T]]
    ev.divide(xs.reduce(ev.plus(_, _)), xs.size)
  }

}
