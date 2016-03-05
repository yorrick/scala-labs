package yorrick.designpatterns

import org.scalatest.{FlatSpec, Matchers, OptionValues, TryValues}
import scala.collection.mutable.Queue


class DecoratorTest extends FlatSpec with Matchers with OptionValues with TryValues {

  "filter decorator" should "filter " in {
    val device = new Queue[String]
    val os = new DeviceOutputStream(device) with Filtering
    os.write("abcdabcd")
    device.dequeueAll(_ => true) shouldBe Seq("bcdbcd")
  }
}
