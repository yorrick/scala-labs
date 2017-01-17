package yorrick.learningscalaz

import scalaz.Applicative
import scalaz.Scalaz._

import scala.language.higherKinds


object ApplicativeTest {
  def main(args: Array[String]): Unit = {
    // Applicative type
    println(1.point[List])

    println(9.some <*> {(_: Int) + 3}.some)

    println(List(1, 2, 3) <*> List((_: Int) * 0, (_: Int) + 100, (x: Int) => x * x))

    println(sequenceA(List(1.some, 2.some)))
    println(sequenceA(List.empty[Option[Int]]))
  }

  // transforms a list of applicative into an applicative of list
  def sequenceA[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] = list match {
    case Nil     => List.empty[A].point[F]  // builds new instance with empty list
    case x :: xs => (x |@| sequenceA(xs)) apply {_ :: _}  // maps Applicative and adds list head
  }
}
