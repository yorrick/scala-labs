package yorrick.learningscalaz

import scalaz._, Scalaz._


object TaggedTypeTest {
  sealed trait KiloGram
  sealed trait JoulePerKiloGram
  def JoulePerKiloGram[A](a: A): A @@ JoulePerKiloGram = Tag[A, JoulePerKiloGram](a)
  def KiloGram[A](a: A): A @@ KiloGram = Tag[A, KiloGram](a)

  def energyR(m: Double @@ KiloGram): Double @@ JoulePerKiloGram =
    JoulePerKiloGram(299792458.0 * 299792458.0 * Tag.unwrap(m))

  def main(args: Array[String]): Unit = {
    val mass = KiloGram(20.0)

    println("======================")
    println(energyR(mass))
  }
}
