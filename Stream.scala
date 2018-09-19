sealed trait Stream[+A] {
  def head: Option[A] = {
    this match {
      case Cons(h, _) => Some(h())
      case Empty => None
    }
  }

  def toList: List[A] = {
    this match {
      case Cons(h, t) => h() :: t().toList
      case Empty => Nil
    }
  }

  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) => if (n == 0) Empty else Cons(h, () => t().take(n - 1))
      case Empty => Empty
    }
  }

  def drop(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) => if (n == 0) this else t().drop(n - 1)
      case Empty => Empty
    }
  }

  // def takeWhile(p: A => Boolean): Stream[A] = {
  //   this match {
  //     case Cons(h, t) =>
  //       if (p(h())) Cons(h, () => t().takeWhile(p)) else Empty
  //     case Empty => Empty
  //   }
  // }

  def foldRight[B](acc: => B)(f: (A, => B) => B): B = {
    this match {
      case Cons(h, t) => f(h(), t().foldRight(acc)(f))
      case Empty => acc
    }
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) || b)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A])((a, b) => if (p(a)) Cons(() => a, () => b) else b)
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](xs: A*): Stream[A] = {
    if (xs.isEmpty) empty else cons(xs.head, apply(xs.tail: _*))
  }

  def const[A](x: A): Stream[A] = {
    cons(x, const(x))
  }

  def unfold[A, S](s: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(s) match {
      case Some((v, ns)) => cons(v, unfold(ns)(f))
      case None => Empty
    }
  }
}

