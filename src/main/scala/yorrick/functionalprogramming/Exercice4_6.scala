package yorrick.functionalprogramming

//hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Either => _, Option => _}


object Exercice4_6 {
  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }

    def orElse[EE >: E, AA >: A](b: Either[EE, AA]): Either[EE, AA] = this match {
      case Right(a) => Right(a)
      case Left(_) => b
    }

    /**
     * Combines 2 either using a function. If any either is a left, returns a left
     * @param bE
     * @param f
     * @tparam EE
     * @tparam B
     * @tparam C
     * @return
     */
    def map2[EE >: E, B, C](bE: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
      a <- this
      b <- bE
    } yield f(a, b)
    
  }
  
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case a :: t => f(a).map2(traverse(t)(f))(_ :: _)
  }
  
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(identity)
  
  def Try[A](a: => A): Either[Exception, A] = 
    try Right(a)
    catch { case e: Exception => Left(e) }
  
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  def main(args: Array[String]): Unit = {
    val age = "45"
    val tickets = "4"
    
    for {
      a <- Try(age.toInt)
      t <- Try(tickets.toInt)
    } yield a / t
    
  }
}
