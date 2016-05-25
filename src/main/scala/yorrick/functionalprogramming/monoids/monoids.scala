package yorrick.functionalprogramming.monoids


trait Monoid[A] {
  def op(a1: A, a2: A): A  // associative operation
  def zero: A
}


object Monoid {
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
}
