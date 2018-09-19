sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Branch(l, r) => size(l) + 1 + size(r)
    case Leaf(_) => 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Branch(l, r) => maximum(l) max maximum(r)
    case Leaf(v) => v
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Branch(l, r) => depth(l) max depth(r) + 1
    case Leaf(_) => 1
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    case Leaf(v) => Leaf(f(v))
  }

  def fold[A, B](t: Tree[A])(bf: (B, B) => B, lf: A => B): B = t match {
    case Branch(l, r) => bf(fold(l)(bf, lf), fold(r)(bf, lf))
    case Leaf(v) => lf(v)
  }
}
