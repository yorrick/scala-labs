package yorrick.functionalprogramming

object PartialFunctionApplication {

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = (b: B) => f(a, b)
  
  case class Email(subject: String, text: String, sender: String, recipient: String)
  
  def sizeConstraint(pred: IntPairPred, n: Int, email: Email): Boolean = pred(email.text.size, n)
  
  type IntPairPred = (Int, Int) => Boolean
  val ge: IntPairPred = _ >= _
  val le: IntPairPred = _ <= _

  // partial function application in scala
  val minimumSize: (Int, Email) => Boolean = sizeConstraint(ge, _: Int, _: Email)
  val maximumSize: (Int, Email) => Boolean = sizeConstraint(le, _: Int, _: Email)

  def main(args: Array[String]): Unit = {
    val add: (Int, Int) => Int = _ + _
    val addOne = partial1(1, add): Int => Int
    println(s"addOne(2): ${addOne(2)}")

    println(s"""minimumSize(3, Email("", "", "", "")): ${minimumSize(3, Email("", "", "", ""))}""")
    println(s"""minimumSize(3, Email("", "tjoijbfg", "", "")): ${minimumSize(3, Email("", "tjoijbfg", "", ""))}""")
  }
}
