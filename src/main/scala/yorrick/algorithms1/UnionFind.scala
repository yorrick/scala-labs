package yorrick.algorithms1

trait UnionFind {
  def size: Int

  /**
   * Stores relations between elements
   */
  protected val ids = Array(0 to size - 1: _*)

  /**
   * Links 2 elements
   */
  def union(p: Int, q: Int)

  /**
   * Find out if 2 elements are connected
   */
  def find(p: Int, q: Int): Boolean
}
