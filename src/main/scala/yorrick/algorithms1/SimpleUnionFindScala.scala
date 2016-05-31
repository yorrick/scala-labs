package yorrick.algorithms1


/**
 * Find: O(1)
 * Union: O(N) problematic because union can be called many times
 */
case class SimpleUnionFindScala(size: Int) extends UnionFind {
  def find(p: Int, q: Int): Boolean = ids(p) == ids(q)

  def union(p: Int, q: Int): Unit = {
    val pid = ids(p)
    val qid = ids(q)

    for (i <- 0 until ids.length) {
      if (ids(i) == pid) ids(i) = qid
    }
  }
}
