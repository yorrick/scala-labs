package yorrick.algorithms1


/**
 * O(N*M) algo 
 */
class TreeUnionFindScala(override val size: Int) extends UnionFind {
  protected def root(i: Int): Int = {
    var element = i
    while (element != ids(element)) {
      element = ids(element)
    }

    return element
  }
  
  def find(p: Int, q: Int): Boolean = root(p) == root(q)
  
  def union(p: Int, q: Int): Unit = {
    val proot = root(p)
    val qroot = root(q)
    rootUnion(proot, qroot)
  }

  protected def rootUnion(proot: Int, qroot: Int): Unit = {
    ids(proot) = qroot
  }
}
