package Chapter10


trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoids {
  // Exercise 10.1
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(i1: Int, i2: Int): Int = i1 + i2
    def zero = 0
  }

  val intMutiplication = new Monoid[Int] {
    def op(i1: Int, i2: Int): Int = i1 * i2
    def zero = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(b1: Boolean, b2: Boolean): Boolean = b1 || b2
    def zero = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(b1: Boolean, b2: Boolean): Boolean = b1 && b2
    def zero = false
  }

  // Exercise 10.2
  // In order to combine two generic types we need a function.
  // The addition of this function breaks the Monoid contract
//  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
//    def op(o1: Option[A], o2: Option[A])(f: (A, A) => A) = (o1, o2) match {
//      case (Some(a), Some(b)) => Some(f(a, b))
//      case (Some(a), None) => Some(a)
//      case (None, Some(b)) => Some(b)
//      case _ => None
//    }
//    def zero = None
//  }


  // Exercise 10.3
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f: A => A, g: A => A): A => A = f andThen g
    def zero = a => a
  }

  // Exercise 10.4
  // This uses the property based framework we created in an earlier chapter. Skip.

  // Exercise 10.5
  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = {
    val bs = as.map { a => f(a) }
    bs.foldRight(m.zero)(m.op)
  }

  // Exercise 10.6
  // This ones very difficult, maybe give it a try later

  // Exercise 10.7
  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.isEmpty) {
      m.zero
    } else if (v.size == 1) {
      f(v.head)
    } else {
      val (left, right) = v.splitAt(v.size)
      val bleft = foldMapV(left, m)(f)
      val bright = foldMapV(right, m)(f)
      m.op(bleft, bright)
    }
  }

  // Exercise 10.8
  // Uses the PAR library we created ealier. Skip

  // Exercise 10.9
  // Take an IndexedSeq, a Monoid and a comparison function
  def isIndexedSeqOrdered[A](v: IndexedSeq[A], m: Monoid[Boolean])(compare: (A, A) => Boolean): Boolean = {
    // Strategy - convert the seq into booleans, where each boolean checks if the element is ordered compared to its
    // right neighbour. Using the AndBoolean monoid we can determine if the entire seq is ordered

    // This seems stupid. Given a compare function we can just fail when it returns false. Why bother with the boolean
    // monoid. I must be missing something, but monoids have no notion of comparison.
    val bools = v.sliding(2).map { case (a, b) => compare(a, b) }
    bools.foldRight(m.zero)(m.op)
  }



  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  // Exercise 10.10
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a: WC, b: WC): WC  = {
      (a, b) match {
        case (Part(l1, w1, r1), Part(l2, w2, r2)) => {
          val addOne = if(r1.nonEmpty && l1.nonEmpty) 1 else 0
          Part(l1, w1 + w2 + addOne, r2)
        }
      }
    }
    def zero = Part("", 0, "")
  }

  def countWords(text: String, m: Monoid[WC]): Int = {
    // Split text until left and right sides reach a maximum length
    // Convert the string to a WC
    // Use the Monoid to combine WCs
    0
  }


}


