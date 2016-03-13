package yorrick.functionalprogramming

import scala.annotation.tailrec

object Exercice3_2 {

  sealed trait List[+A]
  
  case object Nil extends List[Nothing]
  
  case class Cons[+A](head: A, tail: List[A]) extends List[A]
  
  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x,xs) => x + sum(xs)
    }
    
    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x,xs) => x * product(xs)
    }
    
    def tail[A](ds: List[A]): List[A] = ds match {
      case Nil => throw new Exception("should not happen")
      case Cons(_, t) => t
    }
    
    @tailrec
    def drop[A](ds: List[A])(n: Int): List[A] =
      if (n <= 0) ds
      else ds match {
        case Nil => sys.error("cannot drop elements from empty list")
        case Cons(_, t) => drop(t)(n - 1)
      }

    /**
     * Passing predicate in a second argument list help scala compiler for type inference
     * @param ds
     * @param p
     * @tparam A
     * @return
     */
    @tailrec
    def dropWhile[A](ds: List[A])(p: A => Boolean): List[A] = ds match {
      case Cons(h, t) if (p(h)) => dropWhile(t)(p)
      case _ => ds
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }
  
  def main(args: Array[String]): Unit = {
    import List._
    
    val example = Cons(1, Cons(2, Cons(3, Nil)))
    val example2 = List(1,2,3)
    val total = sum(example)
    
    println(s"total: $total")
    println(s"tail(example2): ${tail(example2)}")
    println(s"drop(example2, 2): ${drop(example2)(2)}")
    println(s"dropWhile(example2, 2): ${dropWhile(example2)(_ < 2)}")
  }
}
