package yorrick.algorithms1

import org.scalatest.{FlatSpec, Matchers}


trait QuickBehaviors { this: FlatSpec with Matchers =>

  def findConnectedItems(newQuick: Int => Quick) {
    it should "return connected == true if objects are linked" in {
      val quick = newQuick(10)
      
      quick.size should be >= 10

      quick.union(2, 3)
      quick.union(2, 7)
      quick.union(4, 8)

      quick.connected(3, 7) shouldBe true
      quick.connected(2, 8) shouldBe false
    }
  }
  
  def fastQuickAlgorithm(newQuick: Int => Quick) {
    it should "be able to make a lot a unions in a short time" taggedAs(Benchmark) in {
      val quick = newQuick(1000)
      
      quick.union(2, 3)
      // TODO set seed a generate a lot of links randomly
    }
  }
}

class QuickTest extends FlatSpec with Matchers with QuickBehaviors {
  "A QuickFindUFScala" should behave like findConnectedItems(new QuickFindUFScala(_))
  "A QuickFindUFScala" should behave like fastQuickAlgorithm(new QuickFindUFScala(_))
  
  "A QuickUnionUFScala" should behave like findConnectedItems(new QuickUnionUFScala(_))
  "A QuickUnionUFScala" should behave like fastQuickAlgorithm(new QuickUnionUFScala(_))
}
