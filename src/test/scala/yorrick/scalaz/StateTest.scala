package yorrick.scalaz

import org.scalatest.{FlatSpec, Matchers, OptionValues, TryValues}

import scalaz.{State, _}
import Scalaz._


case class Context(datasets: List[String], broadcasts: List[String])

object Context {
  val empty = Context(List.empty, List.empty)
}


class StateTest extends FlatSpec with Matchers with OptionValues with TryValues {
  "state" should "support State creation" in {
    val m1 = State { s: String => (s, s.size) }
    val (s1, i1) = m1.run("hello")
    s1 shouldBe "hello"
    i1 shouldBe 5

    def repeat(num: Int): State[String, Unit] = State { s: String => (s * num, ()) }
    val (s2, _) = repeat(3).run("abc")
    s2 shouldBe "abcabcabc"
  }

  "state" should "support flatMap" in {
    val m1 = State { s: String => (s, s.size) }
    def repeat(num: Int): State[String, Unit] = State { s: String => (s * num, ()) }

    val (s1, _) = m1.flatMap(repeat).run("hello")
    s1 shouldBe "hellohellohellohellohello"
  }

  "state" should "allow for processing simulation" in {
    import java.util.Random
    def dice() = State[Random, Int](r => (r, r.nextInt(6) + 1))

    def TwoDice() = for {
      r1 <- dice()
      r2 <- dice()
    } yield (r1, r2)

    val result = TwoDice().eval(new Random(1L))
    result shouldBe ((4, 5))

//    val initialState: State[Context, Int => Int] = State(context => (context, identity))
//
////    val processing1: State[Context, Seq[Int]] = State(c => (c.copy(c.datasets +: "processing1 dataset"), Seq.empty))
//
//    initialState.
//
//    val (context, value) = initialState.run(Context.empty)
//
//    context shouldBe Context.empty
//    value shouldBe Seq(1, 2)
  }

  "state" should "allow for processing simulation 2" in {
    import java.util.Random
    def dice() = State[Random, Int](r => (r, r.nextInt(6) + 1))

    def TwoDice() = for {
      r1 <- dice()
      r2 <- dice()
    } yield (r1 + r2)

    val result = TwoDice().eval(new Random(1L))
    result shouldBe 9
  }

  "state" should "allow for processing simulation 3" in {

    def pipe[T](contextHandler: (T, List[T]) => List[T])(f: T => T): State[List[T], T] = State[List[T], T] { context =>
      val value = f(context.head)
      (contextHandler(value, context), value)
    }

    def pipeAndStoreIntermediateResults[T](f: T => T): State[List[T], T] = pipe[T](_ :: _)(f)

    def multiply(by: Int): State[List[Int], Int] = pipeAndStoreIntermediateResults[Int](_ * by)
    def add(a: Int): State[List[Int], Int] = pipeAndStoreIntermediateResults[Int](_ + a)

    def TwoDice() = for {
      r1 <- multiply(2)
      r2 <- add(3)
    } yield (r2)

    val (context, result) = TwoDice().run(List(2))

    context shouldBe List(7, 4, 2)
    result shouldBe 7
  }
}


