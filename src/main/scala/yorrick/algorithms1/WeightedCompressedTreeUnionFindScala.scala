package yorrick.algorithms1

/**
 * O(N + M * log(N)) algo
 */
class WeightedCompressedTreeUnionFindScala(override val size: Int) extends WeightedTreeUnionFindScala(size) {
  
  override protected def root(i: Int): Int = {
    var element = i
    while (element != ids(element)) {
      element = ids(element)
      ids(i) = ids(ids(i))
    }

    return element
  }
}
