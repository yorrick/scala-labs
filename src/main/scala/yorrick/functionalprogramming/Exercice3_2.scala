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
    
    def setHead[A](ds: List[A])(h: A): List[A] = ds match {
      case Nil => List(h)
      case Cons(_, t) => Cons(h, t)
    }

    // not tail recursive, and copies list until end
    def init[A](l: List[A]): List[A] = l match {
      case Nil => sys.error("...")
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    
    @tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => List(buf.toList: _*)
      case Cons(h,t) => buf += h; go(t)
    }
    go(l)
  }
  
  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    println(s"Execution of foldRight, on list $l")
    
    l match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }
  }
  
  def foldRight2[A, B](l: List[A], z: B, shortCircuit: PartialFunction[A, B] = Map.empty)(f: (A, B) => B): B = {
    println(s"Execution of foldRight2, on list $l")
    
    l match {
      case Nil => z
      case Cons(h, t) if shortCircuit.isDefinedAt(h) => shortCircuit(h)
      case Cons(h, t) => f(h, foldRight2(t, z, shortCircuit)(f))
    }
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(h, z))(f)
  }

  def sum3[A](l: List[Int]): Int = foldLeft(l, 0)(_ + _)
  def product3[A](l: List[Double]): Double = foldLeft(l, 0.0)(_ * _)
  def length3[A](l: List[A]): Int = foldLeft(l, 0)((_, acc) => acc + 1)
  
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((a: A, acc: List[A]) => Cons(a, acc))

  def main(args: Array[String]): Unit = {
    import List._
    
    val example = Cons(1, Cons(2, Cons(3, Nil)))
    val example2 = List(1,2,3)
    val total = sum(example)
    
    println(s"total: $total")
    println(s"tail(example2): ${tail(example2)}")
    println(s"drop(example2, 2): ${drop(example2)(2)}")
    println(s"dropWhile(example2, 2): ${dropWhile(example2)(_ < 2)}")
    println(s"foldRight(example2, 1)(_ * _): ${foldRight(example2, 1)(_ * _)}")

    val mutltiplyShortCircuit: PartialFunction[Int, Int] = { case 0 => 0 }
    
    println(s"foldRight2(List(1,0,2,3), 1, mutltiplyShortCircuit)(_ * _): ${foldRight2(List(1,0,2,3), 1, mutltiplyShortCircuit)(_ * _)}")

    val result: List[Int] = foldRight(List(1,2,3), Nil: List[Int])(Cons(_,_))
    
    println(s"length(List(1, 2, 3)): ${length(List(1, 2, 3))}")
   
    
    // stack overflows
    val ints: Seq[Int] = 1 to 1000
//    foldRight(List(ints: _*), 0)(_ + _)

    // does not stack overflows
    println(s"foldLeft(List(ints: _*), 0)(_ + _): ${foldLeft(List(ints: _*), 0)(_ + _)}")
    println(s"foldLeft(List(1, 2, 3), 0)(_ + _): ${foldLeft(List(1, 2, 3), 0)(_ + _)}")
    
    
    println(s"reverse(List(1, 2, 3)): ${reverse(List(1, 2, 3))}")
  }
}
