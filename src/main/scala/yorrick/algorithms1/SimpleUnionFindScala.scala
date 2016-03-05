package yorrick.algorithms1


/**
 * O(N*M) algo
 */
class SimpleUnionFindScala(override val size: Int) extends UnionFind {
  def find(p: Int, q: Int): Boolean = ids(p) == ids(q)
  
  def union(p: Int, q: Int): Unit = {
    val pid = ids(p)
    val qid = ids(q)

    for (i <- 0 until ids.length) {
      if (ids(i) == pid) ids(i) = qid
    }
  }
}
