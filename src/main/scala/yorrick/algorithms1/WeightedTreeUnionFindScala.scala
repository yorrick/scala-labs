package yorrick.algorithms1


/**
 * O(N + M * log(N)) algo
 */
class WeightedTreeUnionFindScala(override val size: Int) extends TreeUnionFindScala(size) {
  private val treeSizes = Array.fill(size)(1)

  override protected def rootUnion(proot: Int, qroot: Int): Unit = {
    if (proot != qroot) {
      if (treeSizes(proot) < treeSizes(qroot)) {
        ids(proot) = qroot
        treeSizes(qroot) += treeSizes(proot)
      } else {
        ids(qroot) = proot
        treeSizes(proot) += treeSizes(qroot)
      }
    }
  }
}
