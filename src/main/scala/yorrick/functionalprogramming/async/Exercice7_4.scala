package yorrick.functionalprogramming.async

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Executors}

import yorrick.functionalprogramming.async.Par._


sealed trait Future[A] {
  private[functionalprogramming] def apply(callback: A => Unit)(eh: Throwable => Unit): Unit
}

case class UnitFuture[A](a: A) extends Future[A] {
  def apply(callback: A => Unit)(eh: Throwable => Unit): Unit = callback(a)  // just run the callback, with no ES
}

class ForkFuture[A](a: => Par[A], es: ExecutorService) extends Future[A] {
  def apply(callback: A => Unit)(eh: Throwable => Unit): Unit = {
    val af: Future[A] = a(es)
    eval(es)(af(callback)(eh))
  }
}

object Par {
  type Par[A] = ExecutorService => Future[A]

  /**
   * Run a Par on a given executor service
   */
  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)

    val af: Future[A] = p(es)
    // set the future's callback
    af.apply { a => ref.set(a); latch.countDown } { case _ => latch.countDown }

    latch.await
    ref.get
  }

  /**
   * Evaluates some action asynchronously, using es
   */
  def eval(es: ExecutorService)(r: => Unit): Unit = es.submit(new Callable[Unit] { def call = r })

  def unit[A](a: A): Par[A] = es => UnitFuture(a)

  def fork[A](a: => Par[A]): Par[A] = es => new ForkFuture(a, es)

  def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] = es => new Future[C] {
    def apply(callback: C => Unit)(eh: Throwable => Unit): Unit = {
      var ar: Option[A] = None
      var br: Option[B] = None

      // this actor awaits for both results, combines them with f and passes the result to callback
      val combiner = Actor[Either[A, B]](es) {
        case Left(a) => br match {
          case None => ar = Some(a)
          case Some(b) => eval(es)(callback(f(a, b)))
        }

        case Right(b) => ar match {
          case None => br = Some(b)
          case Some(a) => eval(es)(callback(f(a, b)))
        }
      }
      
      p(es)(a => combiner ! Left(a))(_ => ())
      p2(es)(b => combiner ! Right(b))(_ => ())
    }
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A, B](a: Par[A])(f: A => B): Par[B] = map2(a, unit(()))((a, _) => f(a))

  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = {
    if (as.isEmpty)
      unit(Vector.empty[A])
    else if (as.length == 1)
      map(as.head)(a => Vector(a))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence[A](as: List[Par[A]]): Par[List[A]] = map(sequenceBalanced(as.toIndexedSeq))(_.toList)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }
}


object Exercice7_4 {

  def main(args: Array[String]): Unit = {
    val es = Executors.newFixedThreadPool(4)

    // here future's callback / continuation is the one set in run()
    println(run(es)(map2(unit(7), unit(9))(_ + _)))
    
    val p = parMap(List.range(1, 1000))(math.sqrt(_))
    val x = run(es)(p)
    println(x)
  }
}
