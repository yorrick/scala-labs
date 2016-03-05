package yorrick.algorithms1

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random
import Math._


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
  
  class PositiveIntGenerator private(max: Int, seed: Int = 1) {
    private val random = new Random(seed)
    def nextInt: Int = abs(random.nextInt) % max 
  }
  
  object PositiveIntGenerator {
    def withMax(max: Int): PositiveIntGenerator = new PositiveIntGenerator(max)
  }
  
  def fastQuickAlgorithm(newQuick: Int => Quick) {
    val quickSize = 100000
    val unionNb = 10000
    
    it should "be able to make a lot a unions in a short time" taggedAs(Benchmark) in {
      val quick = newQuick(quickSize)
      val generator = PositiveIntGenerator.withMax(quickSize)
      val unionPairs: Seq[(Int, Int)] = Seq.fill(unionNb)((generator.nextInt, generator.nextInt))
      
      unionPairs.foreach { case (p, q) =>
        quick.union(p, q)
      }
    }
  }
}

class QuickTest extends FlatSpec with Matchers with QuickBehaviors {
  "A QuickFindUFScala" should behave like findConnectedItems(new QuickFindUFScala(_))
  "A QuickFindUFScala" should behave like fastQuickAlgorithm(new QuickFindUFScala(_))
  
  "A QuickUnionUFScala" should behave like findConnectedItems(new QuickUnionUFScala(_))
  "A QuickUnionUFScala" should behave like fastQuickAlgorithm(new QuickUnionUFScala(_))
}
