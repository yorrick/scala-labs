package yorrick.functionalprogramming

import yorrick.functionalprogramming.RNG.SimpleRNG

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL 
      val nextRNG = SimpleRNG(newSeed) 
      val n = (newSeed >>> 16).toInt 
      (n, nextRNG) 
    }
    
  }
  
  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    
    ((i1, i2), rng3)
  }
  
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }
  
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = 
    if (count > 0) {
      val (i, r) = rng.nextInt
      val rest = ints(count - 1)(r)
      (i :: rest._1, rest._2)
    } else {
      (Nil, rng)
    }

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
      if (count == 0)
        (xs, r)
      else {
        val (x, r2) = r.nextInt
        go(count - 1, r2, x :: xs)
      }
    go(count, rng, List())
  }
  
  
  type Rand[+A] = RNG => (A, RNG)
  
  val int: Rand[Int] = _.nextInt
  
  def unit[A](a: A): Rand[A] = rng => (a, rng)
  
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }
  
  def _double: Rand[Double] = map(int)(i => i / (Int.MaxValue.toDouble + 1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, r2) = ra(rng)
    val (b, r3) = rb(r2)
    (f(a, b), r3)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))
    
  val randIntDouble: Rand[(Int, Double)] = both(int, _double)
  val randDoubleInt: Rand[(Double, Int)] = both(_double, int)
  
  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] = rs match {
    case Nil => unit(Nil)
    case headRand :: tail => rng => {
      val (headResult, r) = headRand(rng)
      val (tailResult, tailRand) = sequence(tail)(r)
      (headResult :: tailResult, tailRand)
    }
  }

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(List[A]())) {
    (f, acc) => map2(f, acc)(_ :: _)
  }

  def intsWithSequence(count: Int): Rand[List[Int]] = sequence2(List.fill(count)(int))
  
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r2) = f(rng)
    val (b, r3) = g(a)(r2)
    (b, r3)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  def _map[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))
  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => _map(rb)(b => f(a, b)))
}


object Test {
  import State._

  case class State[S, +A](run: S => (A, S)) {
    def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
      val (a, newState) = run(s)
      f(a).run(newState)
    })
    
    def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

    def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))
  }

  object State {
    type Rand[A] = State[RNG, A]
    def unit[S, A](a: A): State[S, A] = State(s => (a, s))
  }
  
  def main(args: Array[String]): Unit = {
    case class Person(name: String)
    
    val initialState: State[List[Person], Person] = State.unit(Person("toto"))
    println(initialState.run(Nil))
    println(initialState.map(_.name).run(Nil))

    println(initialState.flatMap(p => State(s => (p, p :: s))).run(Nil))

    def addPerson(p: Person) = State[List[Person], Person](referential => (p, p :: referential))
    println(initialState.flatMap(addPerson).run(Nil))

//    import RNG._
//    
//    val rng = SimpleRNG(42)
//    val (n1, rng2) = rng.nextInt
//    val (n2, rng3) = rng2.nextInt
//    
//    println(n1)  
//    println(n2)
//    
//    println(randomPair(rng))
//    
//    println(ints(6)(rng))
//    
//    println(RNG.unit(3)(rng))
//    println(int(rng))
//    println(map(int)(_ + 1)(rng))
//    println(map(double)(_ + 1)(rng))
//    println(randIntDouble(rng))
//
//    println(sequence(List(int, _double, int, int, int))(rng))
//    println(intsWithSequence(3)(rng))
//    
//    println(flatMap(int)(_ => _double)(rng))
  }
}
