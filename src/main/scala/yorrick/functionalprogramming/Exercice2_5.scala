package yorrick.functionalprogramming

import scala.annotation.tailrec

object Exercice2_5 {
  
  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)
    
    go(n, 1)
  }

  def fib(n: Int): Int = {
    @tailrec
    def loop(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else loop(n - 1, cur, prev + cur)
    
    loop(n, 0, 1)
  }

  def main(args: Array[String]): Unit = {
    println(factorial(7))
    println(fib(7))  // 13
  }
}