package yorrick.functionalprogramming

import scala.annotation.tailrec
import scala.util.Try

//hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Option => _, Either => _, _}


object Exercice4_3 {
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }
    
    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(a) => a
    }
    
    def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None
    def orElse[B >: A](b: => Option[B]): Option[B] = this map (Some(_)) getOrElse b
    def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)
  }
  
  case class Some[+A](a: A) extends Option[A]
  case object None extends Option[Nothing]

  def mean(xs: Seq[Double]): Option[Double] = xs match {
    case _ :: _ => Some(xs.sum / xs.length)
    case Nil => None
  }
  
  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f
//  def lift[A, B](f: A => B): Try[A] => Try[B] = _ map f

  val abs0: Option[Double] => Option[Double] = lift(math.abs)
  
//  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
//    val optAge: Option[Int] = Try(age.toInt)
//  
//  }
  
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(av), Some(bv)) => Some(f(av, bv))
    case _ => None
  }
  
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    val values = a.collect { case Some(v) => v}
    if (values.length < a.length) None else Some(values)
  }
  
  def sequence2[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h.flatMap(hValue => sequence2(t).map(hValue :: _))
  }

  def sequence_1[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil)) { (x,y) => println(s"Calling map2 on $x and $y"); map2(x,y)(_ :: _) }

  def traversePerso[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case a :: t => f(a).flatMap(aRes => traverse(t)(f).flatMap(tailRes => Some(aRes :: tailRes)))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h::t => map2(f(h), traverse(t)(f))(_ :: _)
    }

  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_ :: _))

  def sequenceTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(identity)

  def main(args: Array[String]): Unit = {
    println(mean(Seq(1d, 2.2, 5d)))
    println(mean(Seq.empty))
    
    println(Some(1).map(_ + 1))
    println(None.asInstanceOf[Option[Int]].map(_ + 1))

    println(Some(1).getOrElse(2))
    println(None.asInstanceOf[Option[Int]].getOrElse(2))
    
    println(Some(1).orElse(Some(2)))
    println(None.asInstanceOf[Option[Int]].orElse(Some(2)))
    
    println("variance: " + variance(Seq(1, 3, 5, 2, 3)))

    println(sequence_1(List(Some(1), None, Some(2), Some(3))))
    
    println(traversePerso(List(1, 2, 3))(v => Some(v + 1)))
    println(traverse(List(1, 2, 3))(v => Some(v + 1)))
    println(traverse_1(List(1, 2, 3))(v => Some(v + 1)))
    
    println(traversePerso(List(1, 2, 3))(v => None))
    println(traverse(List(1, 2, 3))(v => None))
    println(traverse_1(List(1, 2, 3))(v => None))
  }
}
