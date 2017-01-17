package yorrick.learningscalaz

import scalaz.Scalaz._
import scalaz._


case class EqualToto(name: String, age: Int)
case class MyCaseClass(foo: Int, bar: String, other: Double)


object EqualTest {

  def main(args: Array[String]): Unit = {
    implicit val EqualTotoEquals: Equal[EqualToto] = new Equal[EqualToto] {
      def equal(a1: EqualToto, a2: EqualToto): Boolean = a1 == a2
    }

//    implicit val myCaseClassHasEquals: Equal[MyCaseClass] = Equal.equalA


    assert(EqualToto("toto", 1) === EqualToto("toto", 1) == true)
    assert(EqualToto("toto", 1) =/= EqualToto("toto", 1) == false)


    1.some =/= 2.some

    // a custom definition of Equal
    implicit val otherEqual: Equal[MyCaseClass] = Equal.equalBy(cc => (cc.foo, cc.other))

    assert(MyCaseClass(1, "sdijvpoxcv", 0) === MyCaseClass(1, "toto", 0) == true)
    assert(MyCaseClass(1, "sdijvpoxcv", 0) === MyCaseClass(1, "sdijvpoxcv", 1) == false)

    val xs = List(MyCaseClass(1, "sdijvpoxcv", 0), MyCaseClass(1, "toto", 0), MyCaseClass(1, "sdijvpoxcv", 1))

    println(xs.indexed)
  }
}
