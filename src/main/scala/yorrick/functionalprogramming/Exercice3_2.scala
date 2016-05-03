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
    l match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }
  }
  
  def foldRight2[A, B](l: List[A], z: B, shortCircuit: PartialFunction[A, B] = Map.empty)(f: (A, B) => B): B = {
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

  def foldLeft4[A,B](l: List[A], z: B)(f: (A, B) => B): B = foldRight(reverse(l), z)(f)

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = foldRight(l, r)(Cons(_,_))

  def append[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_,_))

  def concatenateFlatten[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])(append)

  def addOneTest(l: List[Int]): List[Int] = {
    @tailrec
    def go(cur: List[Int], acc: List[Int]): List[Int] = {
      cur match {
        case Nil => acc
        case Cons(h, t) => go(t, Cons(h + 1, acc))
      }
    }
    go(l, Nil)
  }

  def add1(l: List[Int]): List[Int] = foldRight(l, Nil:List[Int])((h,t) => Cons(h+1,t))
  
  def toStringList(l: List[Double]): List[String] = foldRight(l, Nil:List[String])((h,t) => Cons(h.toString,t))
  
  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((h,t) => Cons(f(h), t))
  
  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil: List[A]) { (h,t) =>
    if (f(h)) Cons(h, t) else t
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, Nil: List[B])((h,t) => append(f(h), t))
  
  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) List(a) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
  }
  
  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }
  
  @tailrec
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = (l, sub) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(hl, tl), Cons(hsub, tsub)) => if (hl == hsub) hasSubsequence(tl, tsub) else hasSubsequence(tl, sub)
  }
  
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(n) => n
    case Branch(l, r) => maximum(l) max maximum(r)
  }
  
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def size2[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(1 + _ + _)
  def depth2[A](tree: Tree[A]): Int = fold(tree)(_ => 0)((b1, b2) => 1 + (b1 max b2))
  def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

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
    println(s"foldLeft(List(1, 2, 3), Nil: List[Int])((i, acc) => Cons(i, acc)): ${foldLeft(List(1, 2, 3), Nil: List[Int])((i, acc) => Cons(i, acc))}")
    println(s"foldRight(List(1, 2, 3), Nil: List[Int])((i, acc) => Cons(i, acc)): ${foldRight(List(1, 2, 3), Nil: List[Int])((i, acc) => Cons(i, acc))}")

    
    println(s"reverse(List(1, 2, 3)): ${reverse(List(1, 2, 3))}")
    println(s"concatenateFlatten(List(List(1, 2), Nil, List(3, 4))): ${concatenateFlatten(List(List(1, 2), Nil, List(3, 4)))}")
    
    println(s"add1(List(1, 2, 3)): ${add1(List(1, 2, 3))}")
    println(s"toStringList(List(1d, 2d, 3d)): ${toStringList(List(1d, 2d, 3d))}")
    println(s"map(List(1, 2, 3))(a => a+1): ${map(List(1, 2, 3))(a => a+1)}")
    println(s"filter(List(1, 2, 3))(_ % 2 == 0): ${filter(List(1, 2, 3))(_ % 2 == 0)}")


    println(flatMap(List(1, 2, 3))(i => List(i, i)))
    
    println(hasSubsequence(List(1, 2, 3, 4), List(1, 2)))
    println(hasSubsequence(List(1, 2, 3, 4), List(2, 3)))
    println(hasSubsequence(List(1, 2, 3, 4), List(4)))
    println(hasSubsequence(List(1, 2, 3, 4), List(4, 3)))
    println(hasSubsequence(List(1, 2, 3, 4), List(1, 1)))
    
    println(size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))))
    println(depth(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(2))), Leaf(3))))
    println(map(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))(_ + 1))

    println(size2(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))))
    println(depth2(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(2))), Leaf(3))))
    println(map2(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))(_ + 1))
  }
}
