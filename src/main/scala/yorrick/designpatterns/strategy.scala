package yorrick.designpatterns


object Strategy {
  type Strategy = (Int, Int) => Int

  class Context(computer: Strategy) {
    def use(a: Int, b: Int): Int =computer(a, b)
  }

  val add: Strategy = _ + _
  val multiply: Strategy = _ * _
}