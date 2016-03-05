package yorrick.algorithms1

import org.scalatest.{FlatSpec, Matchers, OptionValues, TryValues}


class QuickFindTest extends FlatSpec with Matchers with OptionValues with TryValues {

  "Quick find" should "find linked objects" in {
    val qf = new QuickFindUFScala(10)
    qf.union(2, 3)
    qf.union(2, 7)
    qf.union(4, 8)
    
    qf.connected(3, 7) shouldBe true
    qf.connected(2, 8) shouldBe false
  }
}
