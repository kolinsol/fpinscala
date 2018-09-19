object Util {
  def partial[A, B, C](a: A, f: (A, B) => C): B => C = {
    (b: B) => f(a, b)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A, B, C](g: A => B, f: B => C): A => C = {
    (a: A) => f(g(a))
  }
}
