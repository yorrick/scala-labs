package yorrick.designpatterns

import org.scalatest.{FlatSpec, Matchers, OptionValues, TryValues}
import scala.collection.mutable.Queue


class DecoratorTest extends FlatSpec with Matchers with OptionValues with TryValues {

  "filter decorator" should "filter default string" in {
    val device = new Queue[String]
    val os = new DeviceOutputStream(device) with Filtering
    os.write("abcdabcd")
    device.dequeueAll(_ => true) shouldBe Seq("bcdbcd")
  }
  
  "filter decorator" should "filter custom string" in {
    val device = new Queue[String]
    
    // giving parameters to decorator is not as clear as in python
    val os = new DeviceOutputStream(device) with Filtering {
      override val stringToFilter = "b"
    }
    os.write("abcdabcd")
    device.dequeueAll(_ => true) shouldBe Seq("acdacd")
  }
}
