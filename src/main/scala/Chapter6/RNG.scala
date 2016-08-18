package Chapter6


trait RNG {
  def nextInt: (Int, RNG)
//  type State[S,+A] = S => (A,S)
  type Rand[+A] = RNG => (A, RNG)

}

case class SimpleRNG(seed: Long) extends RNG {


  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  val int: Rand[Int] = r => r.nextInt

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val abs = (i: Int) => if(i<0) i * -1 else i
    val (i, rng2) = rng.nextInt
    (abs(i), rng2)
  }

  // stole this from the solns
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (d, rng2) = double(rng)
    val (i, rng3) = rng2.nextInt
    ((i,d), rng3)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1,d2,d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(l: List[Int], c: Int)(rng: RNG): (List[Int], RNG) = {
      if (c > 0) {
        val (i, rng2) = rng.nextInt
        loop(l :+ i, c-1)(rng2)
      } else {
        (l, rng)
      }
    }
    loop(List(), count)(rng)
  }

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)



  def doubleViaMap: Rand[Double] = {
    val f: Int => Double = i => i / (Int.MaxValue.toDouble + 1)
    map(nonNegativeInt)(f)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    def go(retList: List[A], rs: List[Rand[A]], rng: RNG): (List[A], RNG) = {
      rs match {
        case h :: t => {
          val (i, rng2) = h(rng)
          go(retList :+ i, t, rng2)
        }
        case Nil => (retList, rng)
      }
    }
    rng => go(List(), fs, rng)
  }


  def intsViaSequence(count: Int) : Rand[List[Int]] = {
      val l: List[Rand[Int]] = List.fill(count)(rng => rng.nextInt)
      sequence(l)
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
  }

  def mapViaFlatmap[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    def g(a: A): Rand[B] = (rng: RNG) => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
    flatMap(s)(g)
  }

  def map2ViaFlatmap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    def g(a: A): Rand[C] = { rng =>
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }

    flatMap(ra)(g)
  }


}
