package yorrick.functionalprogramming

object Currying {
  
  case class Email(subject: String, text: String, sender: String, recipient: String)
  
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  type IntPairPred = (Int, Int) => Boolean
  val ge: IntPairPred = _ >= _
  val le: IntPairPred = _ <= _
  
  def sizeConstraint(pred: IntPairPred)(n: Int)(email: Email): Boolean = pred(email.text.size, n)

  def main(args: Array[String]): Unit = {
    val add: (Int, Int) => Int = _ + _
    val addFactory: Int => (Int => Int) = curry(add)
    val addTwo = addFactory(2)
    println(s"addTwo(2): ${addTwo(2)}")

    val sizeConstraintFn: IntPairPred => Int => Email => Boolean = sizeConstraint _
    val minSize: Int => Email => Boolean = sizeConstraint(ge)
    val maxSize: Int => Email => Boolean = sizeConstraint(le)

    val min3: Email => Boolean = minSize(3)

    println(s"""min3(Email("", "", "", "")): ${min3(Email("", "", "", ""))}""")
    println(s"""min3(Email("", "tjoijbfg", "", "")): ${min3(Email("", "tjoijbfg", "", ""))}""")
  }
}
