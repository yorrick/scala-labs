package yorrick.functionalprogramming

import java.util.concurrent._
import Par._


object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, unit: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }
  
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val af: Future[A] = a(es)
    val bf: Future[B] = b(es)

    UnitFuture(f(af.get, bf.get))
  }

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    def call = a(es).get
  })
  
  def delay[A](a: => Par[A]): Par[A] = es => a(es)

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A, B](a: Par[A])(f: A => B): Par[B] = map2(a, unit(()))((a, _) => f(a))
  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def sequenceRight[A](as: List[Par[A]]): Par[List[A]] = as match {
    case Nil => unit(Nil)
    case h :: t => map2(h, fork(sequence(t)))(_ :: _)
  }

  def sequenceFoldRight[A](as: List[Par[A]]): Par[List[A]] =
    as.foldRight(unit(List.empty[A]))((a, acc) => map2(a, acc)(_ :: _))

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

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val listOfLists: Par[List[List[A]]] = parMap(as)(a => if (f(a)) List(a) else Nil)
    map(listOfLists)(_.flatten)
  }

  def reduce[A](as: List[A])(f: (A, A) => A): Par[A] = as match {
    case Nil => sys.error("Cannot reduce empty list")
    case h :: Nil => unit(h)
    case h :: t => map2(unit(h), reduce(t)(f))(f)
  }

  def map3[A, B, C, D](aPar: Par[A], bPar: Par[B], cPar: Par[C])(f: (A, B, C) => D): Par[D] = {
    val abPar: Par[(A, B)] = map2(aPar, bPar)((a, b) => (a, b))
    map2(abPar, cPar) { case ((a, b), c) => f(a, b, c) }
  }

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = es => f(a(es).get)(es)

  def parMapReduce[A, B](ps: List[A])(m: A => B)(r: (B, B) => B): Par[B] = {
    val wsPar: Par[List[B]] = parMap(ps)(m)
    flatMap(wsPar)(ws => reduce(ws)(r))
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = p(e).get == p2(e).get
}


object Exercice7_1 {

//  def sum(ints: IndexedSeq[Int]): Par[Int] = {
//    if (ints.size <= 1)
//      Par.unit(ints.headOption getOrElse 0)
//    else {
//      val (l, r) = ints.splitAt(ints.length / 2)
//      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
//    }
//  }
  
  def main(args: Array[String]): Unit = {
    // describes the parallel computation, but does not run it
    val sum: Par[Int] = reduce((1 to 10 by 3).toList)(_ + _)
    // create execution context
    val es = Executors.newFixedThreadPool(10)
    // run par
    val sumF: Future[Int] = sum(es)
    // blocks for result
    println(sumF.get(1, TimeUnit.SECONDS))

    val max: Par[Int] = reduce((1 to 10 by 3).toList)(math.max)
    println(max(es).get(1, TimeUnit.SECONDS))

    val nbWordsPerPar = parMap(List("ovhxcv cbvcbovcb vcbobi", "ovcxv", "cviv cv"))(_.split(" ").length)
    println(nbWordsPerPar(es).get(1, TimeUnit.SECONDS))
    val nbWords = parMapReduce(List("ovhxcv cbvcbovcb vcbobi", "ovcxv", "cviv cv"))(_.split(" ").length)(_ + _)
    println(s"${nbWords(es).get(1, TimeUnit.SECONDS)}")
  }
}
