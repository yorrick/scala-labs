package yorrick.functionalprogramming.parsing

import yorrick.functionalprogramming.testing.{Gen, Prop}
import Prop._

import scala.language.higherKinds
import scala.language.implicitConversions

trait Parser[A]
trait ParseError

trait Parsers[ParseError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]
  def many[A](p: Parser[A]): Parser[List[A]]
  def map[A, B](a: Parser[A])(f: A => B): Parser[B]

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def many(p: Parser[A]): Parser[List[A]] = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
  }

  val sp: Parser[String] = "abra" | "cadabra"

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = forAll(in) { s =>
      run(p1)(s) == run(p2)(s)
    }

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(p, p.map(identity))(in)

    def succeedLaw[A](in: Gen[String]): Prop = forAll(in) { s =>
      run(succeed(s)) == Right(s)
    }
  }
}

object Test {
  def main(args: Array[String]) {
    println("Parsers test")
  }
}