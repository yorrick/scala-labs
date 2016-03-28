package yorrick.functionalprogramming.async

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{CountDownLatch, ExecutorService, Executors}

import yorrick.functionalprogramming.async.Par._


sealed trait Future[A] {
  private[functionalprogramming] def apply(k: A => Unit): Unit
}

object Par {
  type Par[+A] = ExecutorService => Future[A]
  
  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    
    p(es) { a => ref.set(a); latch.countDown }
    latch.await
    ref.get
  }
  
  def unit[A](a: A): Par[A] = es => {
    new Future[A] {
      def apply(callback: A => Unit): Unit = callback(a)
    }
  }
            
}


object Exercice7_4 {

  def main(args: Array[String]): Unit = {
    val es = Executors.newFixedThreadPool(10)
    println(unit(7)(es))
  }
}
