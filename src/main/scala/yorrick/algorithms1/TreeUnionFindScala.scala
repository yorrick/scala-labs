package yorrick.algorithms1


/**
 * Find: O(N)
 * Union: O(N) problematic because union can be called many times
 */
case class TreeUnionFindScala(size: Int) extends UnionFind {
  def find(p: Int, q: Int): Boolean = root(p) == root(q)

  def union(p: Int, q: Int): Unit = {
    val proot = root(p)
    val qroot = root(q)
    rootUnion(proot, qroot)
  }

  /**
   * Find root fo given element
   */
  protected def root(i: Int): Int = {
    var element = i
    while (element != ids(element)) {
      element = ids(element)
    }

    return element
  }

  protected def rootUnion(proot: Int, qroot: Int): Unit = ids(proot) = qroot
}
