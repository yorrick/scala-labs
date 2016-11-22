package yorrick.yarnlogs

import scala.annotation.tailrec

object CollectionImplicits {
  implicit class RichList[T](xs: List[T]) {
    /** Returns shortest possible list of lists xss such that
     *   - xss.flatten == xs
     *   - No sublist in xss contains an element matching p in its tail
     */
    def groupPrefix(p: T => Boolean): List[List[T]] = groupPrefixTailRec(List.empty)(xs)(p)
  }

  @tailrec
  private def groupPrefixTailRec[T](acc: List[List[T]])(xs: List[T])(p: T => Boolean): List[List[T]] = xs match {
    case x :: tail => {
      val (ys, zs) = tail span (!p(_))
      groupPrefixTailRec(acc :+ (x :: ys))(zs)(p)
    }
    case List() => acc
  }
}
