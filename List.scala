sealed trait List[+a]
case object Nil extends List[Nothing]
case class Cons[+A] (head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](xs: A*): List[A] = {
    if (xs.isEmpty) Nil
    else Cons(xs.head, apply(xs.tail: _*))
  }

  def head[A](xs: List[A]): A = xs match {
    case Cons(h, t) => h
  }

  def init[A](xs: List[A]): List[A] = xs match {
    case Cons(h, Cons(_, Nil)) => Cons(h, Nil)
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
    case Nil => Nil
  }

  def tail[A](xs: List[A]): List[A] = xs match {
    case Cons(h, t) => t
  }

  def drop[A](xs: List[A], n: Int): List[A] = xs match {
    case Cons(_, t) => if (n == 0) xs else drop(t, n - 1)
    case Nil => Nil
  }

  def dropWhile[A](xs: List[A])(p: A => Boolean): List[A] = xs match {
    case Cons(h, t) => if (p(h)) dropWhile(t)(p) else xs
    case Nil => Nil
  }

  def append[A](xs: List[A], ys: List[A]): List[A] = xs match {
    case Cons(h, t) => Cons(h, append(t, ys))
    case Nil => ys
  }

  def foldl[A, B](xs: List[A], acc: B)(f: (B, A) => B): B = xs match {
    case Cons(h, t) => foldl(t, f(acc, h))(f)
    case Nil => acc
  }

  def foldr[A, B](xs: List[A], acc: B)(f: (A, B) => B): B = xs match {
    case Cons(h, t) => f(h, foldr(t, acc)(f))
    case Nil => acc
  }

  def reverse[A](xs: List[A]): List[A] = {
    foldl(xs, (Nil: List[A]))((acc, cur) => Cons(cur, acc))
  }

  // def append[A](xs: List[A], ys: List[A]): List[A] = {
  //   foldr(xs, yx)(Cons(_, _))
  // }

  def flatten[A](xs: List[List[A]]): List[A] = {
    foldr(xs, (Nil: List[A]))(append(_, _))
  }

  def map[A, B](xs: List[A])(f: A => B): List[B] = xs match {
    case Cons(h, t) => Cons(f(h), map(t)(f))
    case Nil => Nil
  }

  def filter[A](xs: List[A])(p: A => Boolean): List[A] = xs match {
    case Cons(h, t) => if (p(h)) Cons(h, filter(t)(p)) else filter(t)(p)
    case Nil => Nil
  }

  def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] = {
    flatten(map(xs)(f))
  }

  def zipWith[A, B, C](xs: List[A], ys: List[B])(f: (A, B) => C): List[C] = (xs, ys) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  def hasSub[A](sup: List[A], sub: List[A]): Boolean = {
    def go(xs: List[A], s: List[A]): Boolean = (xs, s) match {
      case (Nil, Nil) => true
      case (Nil, _) => false
      case (_, Nil) => true
      case (Cons(h1, t1), Cons(h2, t2)) =>
        if (h1 == h2) go(t1, t2) else go(t1, sub)
    }

    go(sup, sub)
  }
}
