package yorrick.designpatterns

import org.scalatest.{FlatSpec, Matchers, OptionValues, TryValues}
import LoggerAdapter._


class AdapterTest extends FlatSpec with Matchers with OptionValues with TryValues {

  "logger adapter" should "adapt log objects into logger objects" in {
    val logger: Logger = new Logger
    // warning method is part of Log's API
    logger.warning("Some message")
  }
}
