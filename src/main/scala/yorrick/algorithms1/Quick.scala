package yorrick.algorithms1

trait Quick {
  def size: Int

  /**
   * Stores relations between elements
   */
  protected val ids = Array(0 to size - 1: _*)
  
  def union(p: Int, q: Int)
  def connected(p: Int, q: Int): Boolean
}
