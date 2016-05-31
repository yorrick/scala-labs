package yorrick.algorithms1

/**
 * Find: O(log(size))
 * Union: O(log(size))
 *
 * Balances tree to avoid having trees that are too big, and compresses path
 */
class WeightedCompressedTreeUnionFindScala(override val size: Int) extends WeightedTreeUnionFindScala(size) {
  
  override protected def root(i: Int): Int = {
    var element = i
    while (element != ids(element)) {
      element = ids(element)
      // path compression: assign each element to its grand parent
      ids(i) = ids(ids(i))
    }

    return element
  }
}
