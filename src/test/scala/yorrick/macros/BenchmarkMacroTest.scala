package yorrick.macros

import org.scalatest.{FlatSpec, Matchers, OptionValues, TryValues}


object Test {
  @Benchmark
  def testMethod(base: Double): Double = {
    Math.pow(base, 2)
  }
}

class BenchmarkMacroTest extends FlatSpec with Matchers with OptionValues with TryValues {
  "benchmark annotation" should "print time passed in method" in {
    Test.testMethod(2) shouldBe 4
  }

}


//object MacroTest {
//  def main(args: Array[String]): Unit = {
//    import org.onescience.mr.macros.Mappable
//    import Mappable._
//
//    case class Person(name: String, age: Int)
//
//    //    def mapify[T: Mappable](t: T) = implicitly[Mappable[T]].toMap(t)
//    def mapify[T](t: T) = {
//      val imp = materializeMappable[T]
//      imp.toMap(t)
//    }
//
//    def materialize[T: Mappable](map: Map[String, Any]) = implicitly[Mappable[T]].fromMap(map)
//
//    val person = Person("john", 24)
//
//    assert {
//      mapify(person) == Map("name" -> "john", "age" -> 24)
//    }
//
//    val map = Map("name" -> "bob", "age" -> 22)
//    assert {
//      materialize[Person](map) == Person("bob", 22)
//    }
//  }
//}