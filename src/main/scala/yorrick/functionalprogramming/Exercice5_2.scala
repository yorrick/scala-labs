package yorrick.functionalprogramming
package fpinscala.laziness

import Stream._

import scala.annotation.tailrec

trait Stream[+A] {

  def toListRecursive: List[A] = this match {
    case Cons(h,t) => h() :: t().toListRecursive
    case _ => List()
  }

  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h,t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  def toListFast: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h,t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }
    go(this)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }
  
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }
  
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  // f can choose to to evaluate its second parameter, and stop recursion
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def existsWithFoldRight(p: A => Boolean): Boolean = foldRight(false)((a, b) => {println(s"a: $a"); p(a) || b})
  
  def forAll(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) && b)

  def takeWhileWithFoldRight(p: A => Boolean): Stream[A] = foldRight(empty[A])((h, acc) => if (p(h)) cons(h, acc) else empty)
  
  // starting value None is returned in case Stream is empty
  def headOptionWithFoldRight: Option[A] = foldRight(None: Option[A])((a, _) => Option(a))
  
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h, acc) => cons(f(h), acc))
  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((h, acc) => if (p(h)) cons(h, acc) else acc)
  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((h, acc) => cons(h, acc))
  def flatMap[B >: A](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h, acc) => f(h).append(acc))
  def find(p: A => Boolean): Option[A] = filter(p).headOption
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  
  val ones: Stream[Int] = cons(1, ones)
  
  // This is more efficient than `cons(a, constant(a))` since it's just one object referencing itself
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(prev: Int, cur: Int): Stream[Int] = {
      cons(prev, go(cur, prev + cur))
    }
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty[A]
    case Some((a, newState)) => cons(a, unfold(newState)(f))
  }

  def fibsWithUnfold: Stream[Int] = unfold((0, 1)) { case (prev: Int, cur: Int) => Some((prev, (cur, prev + cur))) }
  def constantWithUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a)))
  def fromWithUnfold(n: Int): Stream[Int] = unfold(n)(s => Some((s, s + 1)))
}


object Test {

  def main(args: Array[String]): Unit = {
    val x = Cons(() => {println("Evaluation of first value"); 1}, () => empty)
    x.headOption
    x.headOption

    println("=================")

    val x2 = Stream({println("Evaluation of first value with smart cons"); 1})
    x2.headOption
    x2.headOption

    println(Stream(1, 2, 3, 4).toList)
    println(Stream(1, 2, 3, 4).take(2))
    println(Stream(1, 2, 3, 4).take(2).toList)
    
    def int(i: Int) = { println(s"i: $i"); i }
    println(Stream(int(1), int(2), int(3), int(4)).takeWhile(_ % 3 != 0).toList)
    
    println(Stream(1, 2, 3, 4).exists(_ % 3 == 0))
    println(Stream(1, 2, 3, 4).existsWithFoldRight(_ % 3 == 0))
    
    println(Stream(1, 2, 3, 4).takeWhileWithFoldRight(_ % 3 != 0).toList)
    
    println(Stream(1, 2, 3, 4).headOptionWithFoldRight)
    println(Stream().headOptionWithFoldRight)
    
    println(Stream(1, 2, 3, 4).map(_ + 1).toList)
    println(Stream(1, 2, 3, 4).filter(_ % 2 == 0).toList)
    
    println(Stream(1, 2).append(Stream(3, 4)).toList)
    println(Stream(1, 2).flatMap(i => Stream(i, i)).toList)
    
    println(ones.take(4).toList)
    println(ones.map(_ + 1).exists(_ % 2 == 0))
    println(ones.forAll(_ != 1))
    
    println(from(4).take(4).toList)
    println(fibs.take(7).toList)
    println(fibsWithUnfold.take(7).toList)
    println(constantWithUnfold(1).take(7).toList)
    println(fromWithUnfold(4).take(7).toList)
  }
}
