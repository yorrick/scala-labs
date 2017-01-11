package yorrick.learningscalaz

import scalaz._


sealed trait KiloGram

object KiloGram {
  def apply[A](a: A): A @@ KiloGram = Tag[A, KiloGram](a)
}

sealed trait JoulePerKiloGram

object JoulePerKiloGram {
  def apply[A](a: A): A @@ JoulePerKiloGram = Tag[A, JoulePerKiloGram](a)

  def energyR(m: Double @@ KiloGram): Double @@ JoulePerKiloGram =
    JoulePerKiloGram(299792458.0 * 299792458.0 * Tag.unwrap(m))

  def energyRInt(m: Int @@ KiloGram): Double @@ JoulePerKiloGram =
    JoulePerKiloGram(299792458.0 * 299792458.0 * Tag.unwrap(m))

  def energyRIntString(m: Int @@ KiloGram): String @@ JoulePerKiloGram =
    JoulePerKiloGram((299792458.0 * 299792458.0 * Tag.unwrap(m)).toString)
}
