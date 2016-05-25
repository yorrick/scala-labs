package yorrick.functionalprogramming.monoids

import yorrick.functionalprogramming.async.Nonblocking.Par


trait Monoid[A] {
  def op(a1: A, a2: A): A  // associative operation
  def zero: A
}


object Monoid {
  // flips a given monoid
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  val stringMonoid = new Monoid[String] {
    def op(s1: String, s2: String): String = s1 + s2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(l1: List[A], l2: List[A]): List[A] = l1 ++ l2
    val zero = List.empty
  }

  val intAddition = new Monoid[Int] {
    def op(i1: Int, i2: Int) = i1 + i2
    val zero = 0
  }

  val intMultiplication = new Monoid[Int] {
    def op(i1: Int, i2: Int) = i1 * i2
    val zero = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(v1: Boolean, v2: Boolean) = v1 || v2
    val zero = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(v1: Boolean, v2: Boolean) = v1 && v2
    val zero = true
  }

  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(v1: Option[A], v2: Option[A]) = v1.orElse(v2)
    val zero = None
  }

  def firstOptionMonoid[A]: Monoid[Option[A]] = optionMonoid[A]
  def lastOptionMonoid[A]: Monoid[Option[A]] = dual(firstOptionMonoid)

  def endoMonoid[A] = new Monoid[A => A] {
    def op(f1: A => A, f2: A => A): A => A = f1 compose f2
    val zero = identity[A]_
  }

  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = as.map(f).fold(m.zero)(m.op)

  // The function type `(A, B) => B`, when curried, is `A => (B => B)`.
  // And of course, `B => B` is a monoid for any `B` (via function composition).
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  // Folding to the left is the same except we flip the arguments to
  // the function `f` to put the `B` on the correct side.
  // Then we have to also "flip" the monoid so that it operates from left to right.
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (v.isEmpty)
      m.zero
    else if (v.length == 1)
      f(v.head)
    else {
      val (left, right) = v.splitAt(v.length / 2)
      m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
    }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2) { case (v1, v2) => m.op(v1, v2)}
    def zero: Par[A] = Par.unit(m.zero)
  }

  // we perform the mapping and the reducing both in parallel
  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.flatMap(Par.parMap(v)(f)){ bs => foldMapV(bs, par(m))(b => Par.async(b => ())) }
}
