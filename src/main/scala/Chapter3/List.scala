package Chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => t
  }

  def setHead[A](l: List[A], head: A): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(head, t)
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if n != 0 => drop(t, n - 1)
    case _ => l
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) =>
      if(f(h)) dropWhile(t, f)
      else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) =>
      if (t == Nil) Nil
      else Cons(h, init(t))
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))


  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((a,b) => 1 + b)
  }

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def sumLeft(ints: List[Int]): Int = {
    foldLeft(ints, 0)((a, b) => a + b)
  }

  def productLeft(ds: List[Double]): Double = {
    foldLeft(ds, 1.0)((a, b) => a * b)
  }

  def lengthLeft[A](as: List[A]): Int = {
    foldLeft(as, 0)((a,b) => a + 1)
  }

  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil: List[A])((a,b) => Cons(b, a))
  }

  def foldRightInTermsOfFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(as, z)((a,b) => f(b,a))
  }

  def lengthRightInTermsOfLeft[A](as: List[A]): Int = {
    foldRightInTermsOfFoldLeft(as, 0)((a,b) => 1 + b)
  }

  def foldLeftInTermsOfFoldRight[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(as, z)((a,b) => f(b,a))
  }

  def lengthLeftInTermsOfRight[A](as: List[A]): Int = {
    foldLeftInTermsOfFoldRight(as, 0)((a,b) => 1 + a)
  }

  def appendRight[A](l: List[A], e: A): List[A] = {
    foldRight(l, Cons(e, Nil))((a,b) => Cons(a, b))
  }

  def appendList[A](as: List[A], bs: List[A]): List[A] = {
    foldRight(as, bs)((a,b) => Cons(a, b))
  }

  def concatLists[A](lists: List[List[A]]): List[A] = {
//    foldRight(reverse(lists), Nil:List[A])( (l, acc) => appendList(acc, l))
    foldRight(lists, Nil:List[A])((a,b) => appendList(a, b))
  }

  def intAddOne(ints: List[Int]): List[Int] = {
    foldRight(ints, Nil:List[Int])((a,b) => Cons(a+1, b))
  }

  def doubleToStringList(doubs: List[Double]): List[String] = {
    foldRight(doubs, Nil:List[String])((a,b) => Cons(a.toString, b))
  }

  def map[A,B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil:List[B])((a,b) => Cons(f(a), b))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil:List[A])((a,b) =>
      if(f(a)) Cons(a, b)
      else b
    )
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
//    foldLeft(as, List():List[B])((a,b) => appendList(a, f(b)))
    foldRight(as, List():List[B])((a,b) => appendList(f(a), b))
  }

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a =>
      if(f(a)) List(a)
      else List()
    )
  }

  def zipInts(as: List[Int], bs: List[Int]): List[(Int, Int)] = {
    (as, bs) match {
      case (Cons(ah, at), Cons(bh, bt)) => Cons((ah, bh), zipInts(at, bt))
      case (Cons(ah, at), Nil) => Cons((ah, 0), zipInts(at, Nil))
      case (Nil, Cons(bh, bt)) => Cons((0, bh), zipInts(Nil, bt))
      case (Nil, Nil) => Nil
    }
  }

  def addListElements(as: List[Int], bs: List[Int]): List[Int] = {
    val tups = zipInts(as, bs)
    foldRight(tups, Nil: List[Int])((a,b) => Cons(a._1 + a._2, b))
  }

  def zipWith[A](as: List[A], bs: List[A])(f: (A,A) => A): List[A] = {
    (as, bs) match {
      case (Cons(ah, at), Cons(bh, bt)) => Cons(f(ah, bh), zipWith(at, bt)(f))
      case (Cons(ah, at), Nil) => Nil
      case (Nil, Cons(bh, bt)) => Nil
      case (Nil, Nil) => Nil
    }
  }



  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???
}