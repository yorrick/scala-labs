package yorrick.learningscalaz


// Monoid type class
trait Monoid[A] {
  def mappend(a1: A, a2: A): A
  def mzero: A
}

object Monoid {
  implicit val IntMonoid: Monoid[Int] = new Monoid[Int] {
    def mappend(a: Int, b: Int): Int = a + b
    def mzero: Int = 0
  }
  
  implicit val StringMonoid: Monoid[String] = new Monoid[String] {
    def mappend(a: String, b: String): String = a + b
    def mzero: String = ""
  }
}

object ExtraMonoids {
  implicit val MultiMonoid: Monoid[Int] = new Monoid[Int] {
    def mappend(a1: Int, a2: Int) = a1 * a2
    def mzero = 1
  }
}
  
object TestMonoid {
  def sum[A: Monoid](xs: List[A]): A = {
    val m = implicitly[Monoid[A]]
    xs.foldLeft(m.mzero)(m.mappend)
  }

  def main(args: Array[String]) {
    println(sum(List(1, 3)))
    println(sum(List("toto", "titi")))

    import ExtraMonoids._
    println(sum(List(1, 3)))
  }
}
