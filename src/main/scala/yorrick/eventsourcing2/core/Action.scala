package yorrick.eventsourcing2.core

import yorrick.eventsourcing2.core.Action._

import scala.annotation.tailrec


/**
 * Action wraps a function that builds (new state, and value) from a given state
 */
private[eventsourcing2] case class Action[S, +A](run: S => (S, A)) {
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
  private[eventsourcing2] def unit[S, A](a: A): Action[S, A] = Action(s => (s, a))

  /**
   * Builds an action from a list of actions
   */
  private[eventsourcing2] def sequence[S, A](sas: List[Action[S, A]]): Action[S, List[A]] = {
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
  private[eventsourcing2] def get[S]: Action[S, S] = Action(s => (s, s))

  /**
   * set action ignores incoming state, and replaces state with the one given. It returns no value (Unit)
   */
  private[eventsourcing2] def set[S](s: S): Action[S, Unit] = Action(_ => (s, ()))

  /**
   * Modify action just read the states, and set it to the value returned by f
   */
  private[eventsourcing2] def modify[S](f: S => S): Action[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  /**
   * Hides action API to make it Input / State oriented
   */
  private[eventsourcing2] def applyUpdates[I, S](z: S)(update: I => S => S)(inputs: List[I]): S = {
    // A => B andThen B => C = A => C
    // Input => (Machine => Machine) andThen (Machine => Machine) => Action[Machine, Unit] = Input => Action[Machine, Unit]
    val inputToActions: I => Action[S, Unit] = update andThen modify[S] _

    // builds an action for each input
    val actions: List[Action[S, Unit]] = inputs.map(inputToActions)

    // combines actions and get into a new action, that returns only coins and candies
    val finalAction = for {
      _ <- sequence(actions)  // builds one action from the list of actions
      s <- get
    } yield s

    finalAction.run(z)._2
  }

  def applyInputs[I, S](update: I => (S => S))(s: S)(inputs: I*): S = {
    applyUpdates(s)(update)(inputs.toList)
  }
}
