package yorrick.functionalprogramming

import scala.annotation.tailrec


object Exercice2_6 {

  def binarySearch[A](as: Array[A], key: A, gt: (A,A) => Boolean): Int = {
    @tailrec
    def go(low: Int, mid: Int, high: Int): Int = {
      if (low > high) -mid - 1
      else {
        val mid2 = (low + high) / 2
        val a = as(mid2)
        val greater = gt(a, key)
        if (!greater && !gt(key,a)) mid2
        else if (greater) go(low, mid2, mid2-1)
        else go(mid2 + 1, mid2, high)
      }
    }
    go(0, 0, as.length - 1)
  }

  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    @tailrec
    def go(cur: Int): Boolean = {
      if (cur >= as.length - 1) true
      else {
        val currentGt = gt(as(cur), as(cur + 1) )
        
        if (!currentGt) false
        else go(cur + 1)
      }
    }
    
    go(0)
  }

  def compose[A,B,C](g: A => B, f: B => C): A => C = (a: A) => f(g(a))
  def compose2[A,B,C](g: A => B, f: B => C): A => C = g andThen f

  def main(args: Array[String]): Unit = {
    val intArray = (1 to 100 by 3).toArray
    println(intArray.toSeq)
    println(binarySearch(intArray, 16, (a: Int, b: Int) => a > b))
    
    println(isSorted(intArray, (a: Int, b: Int) => a < b))
    println(isSorted(intArray, (a: Int, b: Int) => a > b))
    
    val addOne: Int => Int = _ + 1
    val toString: Int => String = i => s"<$i>"
    val addOneToString: Int => String = compose(addOne, toString)
    val addOneToString2: Int => String = compose2(addOne, toString)

    println(s"addOneToString(4): ${addOneToString(4)}")
    println(s"addOneToString2(4): ${addOneToString2(4)}")
  }
}
