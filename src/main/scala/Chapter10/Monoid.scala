package Chapter10

import Chapter3.Tree


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
  // Note this checks iIntegers are in increasing order.
  def isIndexedSeqOrdered(ints: IndexedSeq[Int]): Boolean = {
    // op can compare two ints, but we need a piece of data to keep track if all previous comparisons were successful
    // Use a Monoid[Int, Boolean], op will: if either have false return false, otherwise, check orderness
    // To make this monoid we will have to foldMap converting each int, to int, true
    val creativeMonoid = new Monoid[(Int, Boolean)] {
      override def op(a1: (Int, Boolean), a2: (Int, Boolean)): (Int, Boolean) = {
        if(a1._2 && a2._2) {
          println(s"Comparing ${a1._1} <= ${a2._1}")
          (a1._1, a1._1 <= a2._1) // ordered so far check orderedness
        } else {
          (a1._1, false) // one of the previous orderedness failed, keep going with false
        }
      }
      override def zero: (Int, Boolean) = (Integer.MAX_VALUE, true)
    }

    foldMap[Int, (Int, Boolean)](ints.toList, creativeMonoid)( int => (int, true))._2
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

  // Exercise 10.16
  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A,B)] = {
    new Monoid[(A, B)] {
      override def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
      override def zero: (A, B) = (A.zero, B.zero)
    }
  }

  // Exercise 10.17
  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = {
    new Monoid[(A) => B] {
      override def op(a1: (A) => B, a2: (A) => B): (A) => B = A => B.op(a1(A), a2(A))
      override def zero: (A) => B = A => B.zero
    }
  }

  // Exercise 10.18
  // No idea on this one!
  def bag[A](as: IndexedSeq[A]): Map[A, Int] = ???

}

trait Foldable[F[_]] {
  def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B
  def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B
  def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)
  // Exericse 10.15
  def toList[A](fa: F[A]): List[A] = foldRight(fa)(List[A]())( (a,b) => a :: b)
}


class FoldableList extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: (A) => B)(mb: Monoid[B]): B = foldLeft(as)(mb.zero)((b,a) => mb.op(b, f(a)))
}


class FoldableVector extends Foldable[Vector] {
  override def foldRight[A, B](as: Vector[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
  override def foldLeft[A, B](as: Vector[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
  override def foldMap[A, B](as: Vector[A])(f: (A) => B)(mb: Monoid[B]): B = foldLeft(as)(mb.zero)((b,a) => mb.op(b, f(a)))
}


class FoldableStream extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
  override def foldMap[A, B](as: Stream[A])(f: (A) => B)(mb: Monoid[B]): B = foldLeft(as)(mb.zero)((b,a) => mb.op(b, f(a)))
}

// Exercise 10.13
//class FoldableTree extends Foldable[Tree] {
  // The old foldRight requires two functions, A => B and (B,B) => B, but monoid only supplies (A,B) => B
//  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = Tree.foldRight(as)
//  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
//  override def foldMap[A, B](as: Tree[A])(f: (A) => B)(mb: Monoid[B]): B = foldLeft(as)(mb.zero)((b,a) => mb.op(b, f(a)))
//}


// Exercise 10.14
class FoldableOption extends Foldable[Option] {
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
    case None => z
    case Some(x) => f(x, z)
  }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
    case None => z
    case Some(x) => f(z, x)
  }

  override def foldMap[A, B](as: Option[A])(f: (A) => B)(mb: Monoid[B]): B = as match {
    case None => mb.zero
    case Some(x) => mb.op(f(x), mb.zero)
  }
}




