package yorrick.yarnlogs

object Implicits {
  implicit class RichString(s: String) {
    def i: String = "^.*" + s + ".*$"
  }
}
