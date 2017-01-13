package yorrick.learningscalaz


import scalaz._
import Scalaz._


case class Pole(left: Int, right: Int) {
  def landLeft(n: Int): Option[Pole] =
  if (math.abs((left + n) - right) < 4) copy(left = left + n).some
  else none

  def landRight(n: Int): Option[Pole] =
  if (math.abs(left - (right + n)) < 4) copy(right = right + n).some
  else none
}
