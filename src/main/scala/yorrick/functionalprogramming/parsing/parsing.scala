package yorrick.functionalprogramming.parsing

import yorrick.functionalprogramming.testing.{Gen, Prop}
import Prop._

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.util.matching.Regex

trait Parser[A]
trait ParseError

trait Parsers[ParseError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  def slice[A](p: Parser[A]): Parser[String]
  def wrap[A](p: => Parser[A]): Parser[A]
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, p.many)(_ :: _)
  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, wrap(many(p)))(_ :: _) or succeed(List())
  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] = flatMap(p)(a => map(p2)(b => (a, b)))
  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = flatMap(p)(a => p2.map(b => f(a, b)))
  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = flatMap(p)(a => succeed(f(a)))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n > 0) map2(p, listOfN(n - 1, p))(_ :: _)
    else succeed(List())

  implicit def string(s: String): Parser[String]
  implicit def regex(r: Regex): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def many: Parser[List[A]] = self.many(p)
    def many1: Parser[List[A]] = self.many1(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
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

    def unbiasL[A,B,C](p: ((A,B), C)): (A,B,C) = (p._1._1, p._1._2, p._2)
    def unbiasR[A,B,C](p: (A, (B,C))): (A,B,C) = (p._1, p._2._1, p._2._2)

    def productLaw[A, B, C](a: Parser[A], b: Parser[B], c: Parser[C])(in: Gen[String]): Prop = forAll(in) { s =>
      // associativity
      run((a ** b) ** c map (unbiasL))(s) == run(a ** (b ** c) map (unbiasR))(s)
      // a.map(f) ** b.map(g) == (a ** b) map { case (a,b) => (f(a), g(b)) }
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


      val intParser: Parser[Int] = ("[0-9]+".r).map(_.toInt)
      intParser.flatMap(n => listOfN(n, char('a')))
    }
  }
}

object Test {
  def main(args: Array[String]) {
    println("Parsers test")
  }
}