package yorrick.functionalprogramming.parsing

import scala.util.matching.Regex

class MyParser[+A]() {

}

object MyParsers extends Parsers[MyParser] {
  type Parser[+A] = Location => Result[A]

  trait Result[+A]
  case class Success[+A](get: A, charConsumed: Int) extends Result[A]
  case class Failure(get: ParseError) extends Result[Nothing]

  def string(s: String): Parser[String] = location => {
    if (location.input.startsWith(s)) Success(s, s.length)
    else Failure(Location(location.input).toError("Expected: " + s))
  }

  def regex(r: Regex): Parser[String] = location => location.input match {
    case r(value) => Success(value, value.length)
    case _ => Failure(Location(location.input).toError("Expected regex: " + r))
  }
}

