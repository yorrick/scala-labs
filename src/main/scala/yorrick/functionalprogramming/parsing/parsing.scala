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
  def slice[A](p: Parser[A]): Parser[String]
  def product[A, B](p: Parser[A], p2: Parser[B]): Parser[(A, B)]

  def map2[A, B, C](p: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] = product(p, p2).map { case (a, b) => f(a, b)}
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, p.many)(_ :: _)

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def many: Parser[List[A]] = self.many(p)
    def many1: Parser[List[A]] = self.many1(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def slice: Parser[String] = self.slice(p)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def map2[B, C](p2: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = forAll(in) { s =>
      run(p1)(s) == run(p2)(s)
    }

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(p, p.map(identity))(in)

    def succeedLaw[A](in: Gen[String]): Prop = forAll(in) { s =>
      run(succeed(s))("anything at all") == Right(s)
    }

    def tests: Unit = {
      val sp: Parser[String] = "abra" | "cadabra"
      val ip: Parser[Int] = sp.map(_.length)
      val spm: Parser[List[String]] = sp.many
      run(slice(("a" | "b").many))("aaba") == Right("aaba")

      val cp: Parser[Char] = char('t')
      val cpm: Parser[List[Char]] = char('t').many

      char('a').many.slice.map(_.size)

      val manyAMany1B: Parser[(Int, Int)] = char('a').many.slice.map(_.size) ** char('b').many1.slice.map(_.size)
    }
  }
}

object Test {
  def main(args: Array[String]) {
    println("Parsers test")
  }
}