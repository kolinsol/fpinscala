trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  type Rand[+A] = RNG => (A, RNG)

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (r, rng1) = s(rng)
      (f(r), rng1)
    }
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (r1, rng1) = ra(rng)
      val (r2, rng2) = rb(rng1)
      (f(r1, r2), rng2)
    }
  }

  def sequence[A](xs: List[Rand[A]]): Rand[List[A]] = {
    def go(xs: List[Rand[A]], acc: List[A], rng: RNG): (List[A], RNG) = {
      xs match {
        case (h :: t) => {
          val (v, newRng) = h(rng)
          go(t, v :: acc, newRng)
        }
        case Nil => (acc.reverse, rng)
      }
    }

    rng => {
      go(xs, Nil, rng)
    }
  }

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = {
    rng => {
      val (r1, rng1) = r(rng)
      val r2 = f(r1)
      r2(rng1)
    }
  }

  def ints(count: Int, rng: RNG): List[Int] = {
    count match {
      case 0 => Nil
      case n =>
        val (n1, rng1) = rng.nextInt
        n1 :: ints(n - 1, rng1)
    }
  }
}
