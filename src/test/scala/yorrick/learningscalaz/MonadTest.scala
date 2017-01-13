package yorrick.learningscalaz

import org.scalatest.{FlatSpec, Matchers, OptionValues, TryValues}

import scalaz._
import Scalaz._


class MonadTest extends FlatSpec with Matchers with OptionValues with TryValues {
  "option monad" should "provide point and flatmap" in {
    // point is a constructor provided by Applicative type class
    Monad[Option].point(Pole(0, 0)) flatMap {_.landRight(2)} flatMap {_.landLeft(2)} flatMap {_.landRight(2)} shouldBe Some(Pole(2, 4))

    (Monad[Option].point(Pole(0, 0)) >>= {_.landLeft(1)} >>= {_.landRight(4)} >>= {_.landLeft(-1)} >>= {_.landRight(-2)}) shouldBe None
  }

  "option monad" should "be written using for" in {
    def routine: Option[Pole] =
      for {
        start <- Monad[Option].point(Pole(0, 0))
        first <- start.landLeft(2)
        _ <- (none: Option[Pole])
        second <- first.landRight(2)
        third <- second.landLeft(1)
      } yield third

    routine shouldBe none
  }

  "for" should "allow pattern matching" in {
    def justH: Option[Char] =
      for {
        (x :: xs) <- "hello".toList.some
      } yield x

    justH.value shouldBe 'h'
  }

  "for" should "allow pattern matching, and return empty (fail) value if no match is found" in {
    def wopwop: Option[Char] =
      for {
        (x :: xs) <- "".toList.some
      } yield x

    wopwop should be (none)
  }


}
