package yorrick.learningscalaz

import scalaz.syntax.Ops


// Type class for types that support 'map'
trait Functor[F[_]] {
  /** Lift `f` into `F` and apply to `F[A]`. */
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait FunctorOps[F[_], A] extends Ops[F[A]] {
  implicit def F: Functor[F]
  final def map[B](f: A => B): F[B] = F.map(self)(f)
}


object FunctorTest {
  def main(args: Array[String]): Unit = {
    List(1, 2, 3) map {_ + 1}

    import scalaz.Scalaz._
    // Scalaz also defines Functor instance for Function1.
    val myFunction: Int => String = ((x: Int) => x + 1) map {_ * 7} map { _.toString }

    println(myFunction(3))

    println(List(1, 2, 3) >| "x")
  }
}
