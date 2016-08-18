package Chapter5

import Stream._

import scala.util.Try


sealed trait Stream[+A] {
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case Empty => Nil
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if n <= 0 => Empty
    case Cons(h, t) => cons(h(), t().take(n-1))
  }

  def takeViaUnfold(n: Int): Stream[A] = {
    unfold[A, (Int,Stream[A])]((n,this)) {
      case (i, Cons(h,t)) if i > 0 => Some(h(), (i-1, t()))
      case _ => None
    }
  }


  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if n <= 1 => t()
    case Cons(h, t) => t().drop(n-1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean = {
    foldRight(false)((a, b) =>  p(a) || b )
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a,b) => p(a) && b)
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = {
    foldRight[Stream[A]](Empty)((a,b) =>
      if(p(a)) cons(a, b)
      else b
    )
  }

  def headOption: Option[A] = {
    foldRight[Option[A]](None)((a,b) => Some(a))
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight[Stream[B]](Empty)((h,t) => cons(f(h), t))
  }

  def mapViaUnfold[B](f: A => B): Stream[B] = {
    val g: Stream[A] => Option[(B, Stream[A])] = {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    }
    unfold(this)(g)
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight[Stream[A]](Empty)((h,t) =>
      if(f(h)) cons(h, t.filter(f))
      else t.filter(f)
    )
  }

  def append[B >: A](z: Stream[B]): Stream[B] = {
    foldRight(z)((h,t) => cons(h, t))
  }

  def flatMap[B](f: A => Stream[B]) = {
    foldRight[Stream[B]](Empty)((h,t) => f(h).append(t))
  }

  def tails: Stream[Stream[A]] = {
    unfold(this) {
      case s @ Cons(h, t) => Some(s, t())
      case Empty => None
    }
  }

  def zipWith[B,C](s: Stream[B])(f: (A,B) => C): Stream[C] = {
    unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = {
    unfold(this, s2) {
      case (Cons(h1,t1), Cons(h2,t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Empty, Cons(h2,t2)) => Some((None, Some(h2())), (Empty, t2()))
      case (Cons(h1,t1), _) => Some((Some(h1()), None), (t1(), Empty))
      case _ => None
    }
  }

  def startsWith[B >: A](s: Stream[B]): Boolean = {
    val f: (B,B) => Boolean = (a1,a2) => a1 == a2
    this.zipWith(s)(f).forAll(_ == true)
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = {
    Stream.cons(a, constant(a))
  }

  def from(n: Int): Stream[Int] = {
    Stream.cons(n, from(n+1))
  }

  def fibs(n0: Int = 1, n1: Int = 1): Stream[Int] = {
    Stream.cons(n0, fibs(n1, n0+n1))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
      case None => Empty
    }
  }

  def constantViaUnfold[A](a: A): Stream[A] = {
    val f: A => Option[(A, A)] = a => Some((a, a))
    unfold(a)(f)
  }

  def fromViaUnfold(n: Int): Stream[Int] = {
    val f: Int => Option[(Int, Int)] = a => Some((a, a+1))
    unfold(n)(f)
  }

//  def fibsViaUnfold: Stream[Int] = {
//    val f: ((Int, Int)) => Option[(Int, (Int,Int))] =
//      (n0:Int, n1:Int) => Some((n0, (n1, n0+n1)))
//
//    unfold((1,1))(f)
//
//  }

  def primes: Stream[Int] = {
    val filters: Seq[Int => Boolean] = Seq()
    val newfilter: Int => Int => Boolean = a => b => b % a == 0

    cons(1, unfold((2, filters)) {
      case (i, fs) =>
        def go(a: Int, ffs: Seq[Int => Boolean]): (Int, (Int, Seq[Int=>Boolean])) = {
          if(ffs.exists { f => f(a) } ) go(a+1, ffs)
          else (a, (a+1, ffs :+ newfilter(a)))
        }
        Some(go(i, fs))
    })
  }
}