sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(v) => Some(f(v))
    case None => None
  }
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(v) => f(v)
    case None => None
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(v) => v
    case None => default
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(_) => this
    case None => ob
  }
  def filter(p: A => Boolean): Option[A] = this match {
    case Some(v) if (p(v)) => this
    case None => None
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch {
      case e: Exception => None
    }
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    (a, b) match {
      case (None, None) => None
      case (None, _) => None
      case (_, None) => None
      case (Some(a), Some(b)) => Some(f(a, b))
    }
  }

  def sequence[A](xs: List[Option[A]]): Option[List[A]] = {
    def go(xs: List[Option[A]], acc: List[A]): Option[List[A]] = {
      xs match {
        case (Some(v) :: t) => go(t, v :: acc)
        case (None :: t) => None
        case Nil => Some(acc.reverse)
      }
    }

    go(xs, Nil)
  }

  def traverse[A, B](xs: List[A])(f: A => Option[B]): Option[List[B]] = {
    def go(xs: List[A], acc: List[B]): Option[List[B]] = {
      xs match {
        case x :: t =>
          f(x) match {
            case Some(v) => go(t, v :: acc)
            case None => None
          }
        case Nil => Some(acc.reverse)
      }
    }

    go(xs, Nil)
  }
}
