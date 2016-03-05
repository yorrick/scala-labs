package yorrick.algorithms1

trait UnionFind {
  def size: Int

  /**
   * Stores relations between elements
   */
  protected val ids = Array(0 to size - 1: _*)
  
  def union(p: Int, q: Int)
  def find(p: Int, q: Int): Boolean
}
