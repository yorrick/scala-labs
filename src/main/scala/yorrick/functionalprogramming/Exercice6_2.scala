package yorrick.functionalprogramming

import yorrick.functionalprogramming.RNG.SimpleRNG
import Action._
import Candy._

import scala.annotation.tailrec


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


/**
 * Action wraps a function that builds (new state, and value) from a given state
 */
case class Action[S, +A](run: S => (S, A)) {
  /**
   * Adds an action to this action. 
   * flatMap chains this action to another one, using a function that builds another action
   */
  def flatMap[B](f: A => Action[S, B]): Action[S, B] = Action(s => {
    val (newState, a) = run(s)
    f(a).run(newState)
  })

  /**
   * Adds a function to this action. 
   * map builds another action by transforming this action's resulting value, but does not touch state (use unit)
   */
  def map[B](f: A => B): Action[S, B] = flatMap(a => unit(f(a)))

  /**
   * Composes 2 actions' results using a function
   */
  def map2[B,C](sb: Action[S, B])(f: (A, B) => C): Action[S, C] = flatMap(a => sb.map(b => f(a, b)))
}


object Action {
  /**
   * Build an action that has no effect on state
   */
  def unit[S, A](a: A): Action[S, A] = Action(s => (s, a))

  /**
   * Builds an action from a list of actions
   */
  def sequence[S, A](sas: List[Action[S, A]]): Action[S, List[A]] = {
    @tailrec
    def go(state: S, actions: List[Action[S, A]], valueAcc: List[A]): (S, List[A]) =
      actions match {
        case Nil => (state, valueAcc.reverse)
        case headStateAction :: tail => headStateAction.run(state) match { 
          case (newState, value) => go(newState, tail, value :: valueAcc)
        }
      }
    
    Action((s: S) => go(s, sas, List.empty))
  }

  /**
   * get action just passes the state along, and also returns state
   */
  def get[S]: Action[S, S] = Action(s => (s, s))

  /**
   * set action ignores incoming state, and replaces state with the one given. It returns no value (Unit)
   */
  def set[S](s: S): Action[S, Unit] = Action(_ => (s, ()))
  
  /**
   * Modify action just read the states, and set it to the value returned by f
   */
  def modify[S](f: S => S): Action[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()
}


sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)


object Candy {
  
  /**
   * From an input, builds a state transition (Machine => Machine function)
   */
  def update: Input => (Machine => Machine) = input => machine => (input, machine) match {
    case (_, Machine(_, 0, _)) => machine
    case (Coin, Machine(false, _, _)) => machine
    case (Turn, Machine(true, _, _)) => machine
    case (Coin, Machine(true, candy, coin)) => Machine(false, candy, coin + 1)
    case (Turn, Machine(false, candy, coin)) => Machine(true, candy - 1, coin)
  }

  /**
   * Builds an action from a list of inputs
   */
  def simulateMachine(inputs: List[Input]): Action[Machine, (Int, Int)] = {
    // A => B andThen B => C = A => C
    // Input => (Machine => Machine) andThen (Machine => Machine) => Action[Machine, Unit] = Input => Action[Machine, Unit]
    val inputToActions: Input => Action[Machine, Unit] = update andThen modify[Machine] _
    
    // builds an action for each input
    val actions: List[Action[Machine, Unit]] = inputs.map(inputToActions)
    
    // builds one action from the list of actions
    val bigAction = sequence(actions)

    // combines bigAction and get into a new action, that returns only coins and candies
    for {
      _ <- bigAction
      s <- get
    } yield (s.candies, s.coins)
  }
  
  def applyInputs(machine: Machine)(inputs: Input*): (Machine, (Int, Int)) = {
    simulateMachine(inputs.toList).run(machine)
  }
}


object Test {
  
  def main(args: Array[String]): Unit = {
    println(applyInputs(Machine(false, 10, 3))(Turn))
    println(applyInputs(Machine(false, 0, 3))(Turn, Coin, Turn, Coin))
    println(applyInputs(Machine(false, 10, 3))(Coin, Coin, Turn, Coin))
    println(applyInputs(Machine(false, 10, 3))(Coin, Coin, Turn, Coin, Turn))
    println(applyInputs(Machine(true, 10, 3))(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))

    // command, state => Opt[event]: not a pb
    // events, state => state: solved
    
//    case class Person(name: String)
//    
//    val initialState: State[List[Person], Person] = State.unit(Person("toto"))
//    println(initialState.run(Nil))
//    println(initialState.map(_.name).run(Nil))
//
//    println(initialState.flatMap(p => State(s => (p, p :: s))).run(Nil))
//
//    def addPerson(p: Person) = State[List[Person], Person](referential => (p, p :: referential))
//    println(initialState.flatMap(addPerson).run(Nil))


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
