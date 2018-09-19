sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Left(e) => Left(e)
      case Right(v) => Right(f(v))
    }
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either [EE, B] = {
    this match {
      case Right(v) => f(v)
      case Left(e) => Left(e)
    }
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(_) => b
      case Right(v) => Right(v)
    }
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def Try[A](a: => A): Either[Exception, A] = {
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }
  }

  def map2[E, A, B, C](a: Either[E, A], b: Either[E, B])(f: (A, B) => C): Either[E, C] = {
    (a, b) match {
      case (Left(e), _) => Left(e)
      case (_, Left(e)) => Left(e)
      case (Right(v1), Right(v2)) => Right(f(v1, v2))
    }
  }

  def sequence[E, A](xs: List[Either[E, A]]): Either[E, List[A]] = {
    def go(xs: List[Either[E, A]], acc: List[A]): Either[E, List[A]] = {
      xs match {
        case (Right(v) :: t) => go(t, v :: acc)
        case (Left(e) :: t) => Left(e)
        case Nil => Right(acc.reverse)
      }
    }

    go(xs, Nil)
  }

  def traverse[E, A, B](xs: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    def go(xs: List[A], acc: List[B]): Either[E, List[B]] = {
      xs match {
        case (h :: t) =>
          f(h) match {
            case Right(v) => go(t, v :: acc)
            case Left(e) => Left(e)
          }
        case Nil => Right(acc.reverse)
      }
    }

    go(xs, Nil)
  }
}
