package yorrick.learningscalaz

import ToCanIsTruthyOps._


// type class
trait CanTruthy[A] { self =>
  /** @return true, if `a` is truthy. */
  def truthys(a: A): Boolean
}

// constructors of type class
object CanTruthy {
  def apply[A](implicit ev: CanTruthy[A]): CanTruthy[A] = ev
  def truthys[A](f: A => Boolean): CanTruthy[A] = new CanTruthy[A] {
    def truthys(a: A): Boolean = f(a)
  }
}

//trait CanTruthyOps[A] {
//  def self: A
//  implicit def F: CanTruthy[A]
//  final def truthy: Boolean = F.truthys(self)
//}
//
//object ToCanIsTruthyOps {
//  implicit def toCanIsTruthyOps[A](v: A)(implicit ev: CanTruthy[A]) = new CanTruthyOps[A] {
//    def self = v
//    implicit def F: CanTruthy[A] = ev
//  }
//}

// simpler version of previous commented code
object ToCanIsTruthyOps {
  implicit class CanTruthyOps[A: CanTruthy](a: A) {
    final def truthy: Boolean = implicitly[CanTruthy[A]].truthys(a)
  }
}


object CanTruthyTest {
  def main(args: Array[String]): Unit = {
    implicit val intCanTruthy: CanTruthy[Int] = CanTruthy.truthys({
      case 0 => false
      case _ => true
    })

    println(10.truthy)


  }
}
