package yorrick.yarnlogs

object CollectionImplicits {
  implicit class RichList[T](xs: List[T]) {
    /** Returns shortest possible list of lists xss such that
     *   - xss.flatten == xs
     *   - No sublist in xss contains an element matching p in its tail
     *  TODO make that tail recursive!
     */
    def groupPrefix(p: T => Boolean): List[List[T]] = xs match {
      case List() => List()
      case x :: xs1 =>
        val (ys, zs) = xs1 span (!p(_))
        (x :: ys) :: zs.groupPrefix(p)
    }
  }
}
