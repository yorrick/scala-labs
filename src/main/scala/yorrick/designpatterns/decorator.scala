package yorrick.designpatterns

import scala.collection.mutable.Queue


trait OutputStream {
  def write(s: String)
}

class DeviceOutputStream(device: Queue[String]) extends OutputStream {
  def write(s: String) {
    device += s
  }
}

trait Filtering extends OutputStream {
  private val stringToFilter = "a"
  
  abstract override def write(s: String) {
    super.write(s.replaceAll(stringToFilter, ""))
  }
}
