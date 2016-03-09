package yorrick.algorithms1

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random
import Math._


trait UnionFindBehaviors { this: FlatSpec with Matchers =>

  def findConnectedItems(newQuick: Int => UnionFind) {
    it should "return connected == true if objects are linked" in {
      val quick = newQuick(10)
      
      quick.size should be >= 10

      quick.union(2, 3)
      quick.union(2, 7)
      quick.union(4, 8)

      quick.find(3, 7) shouldBe true
      quick.find(2, 8) shouldBe false
    }
  }
  
  class PositiveIntGenerator private(max: Int, seed: Int = 1) {
    private val random = new Random(seed)
    def nextInt: Int = abs(random.nextInt) % max 
  }
  
  object PositiveIntGenerator {
    def withMax(max: Int): PositiveIntGenerator = new PositiveIntGenerator(max)
    
    def pairSequence(size: Int, max: Int): Seq[(Int, Int)] = {
      val generator = PositiveIntGenerator.withMax(max)
      Seq.fill(size)((generator.nextInt, generator.nextInt))
    }
  }
  
  def fastUnionAlgorithm(newQuick: Int => UnionFind) {
    it should "be able to make a lot a unions in a short time" taggedAs(Benchmark) in {
      val quick = newQuick(100000)
      
      PositiveIntGenerator.pairSequence(10000, quick.size).foreach { 
        case (p, q) => quick.union(p, q) 
      }
    }
  }
  
  def fastFindAlgorithm(newQuick: Int => UnionFind) {
    it should "be able to make a lot a finds in a short time" taggedAs(Benchmark) in {
      val quick = newQuick(1000000)
      
      PositiveIntGenerator.pairSequence(10000, quick.size).foreach { 
        case (p, q) => quick.union(p, q) 
      }

      PositiveIntGenerator.pairSequence(1000000, quick.size).foreach {
        case (p, q) => quick.find(p, q)
      }
    }
  }
}

class UnionFindTest extends FlatSpec with Matchers with UnionFindBehaviors {
  "A QuickFindUFScala" should behave like findConnectedItems(new SimpleUnionFindScala(_))
  "A QuickFindUFScala" should behave like fastUnionAlgorithm(new SimpleUnionFindScala(_))
  "A QuickFindUFScala" should behave like fastFindAlgorithm(new SimpleUnionFindScala(_))

  "A QuickUnionUFScala" should behave like findConnectedItems(new TreeUnionFindScala(_))
  "A QuickUnionUFScala" should behave like fastUnionAlgorithm(new TreeUnionFindScala(_))
  "A QuickUnionUFScala" should behave like fastFindAlgorithm(new TreeUnionFindScala(_))

  "A WeightedQuickUnionUFScala" should behave like findConnectedItems(new WeightedTreeUnionFindScala(_))
  "A WeightedQuickUnionUFScala" should behave like fastUnionAlgorithm(new WeightedTreeUnionFindScala(_))
  "A WeightedQuickUnionUFScala" should behave like fastFindAlgorithm(new WeightedTreeUnionFindScala(_))
  
  "A WeightedCompressedTreeUnionFindScala" should behave like findConnectedItems(new WeightedCompressedTreeUnionFindScala(_))
  "A WeightedCompressedTreeUnionFindScala" should behave like fastUnionAlgorithm(new WeightedCompressedTreeUnionFindScala(_))
  "A WeightedCompressedTreeUnionFindScala" should behave like fastFindAlgorithm(new WeightedCompressedTreeUnionFindScala(_))
}
