package Chapter4

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(v) => Some(f(v))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(v) => f(v)
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(v) => v
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(v) => this
    case None => ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(v) if f(v) => this
    case _ => None
  }


}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap { m =>
      mean(xs.map { x => math.pow(x - m, 2) })
    }
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      o1 <- a
      o2 <- b
    } yield f(o1, o2)
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case b :: bs => b.flatMap { c => sequence(bs).map { d => c :: d } }
      case Nil => Some(Nil)
    }
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(Nil))((a,b) => map2(a, b)(_ :: _))
  }

  // Oops, didnt need to flatmap over b
  // reimplemented map2, lol
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(List()))((a,b) => b.flatMap( bb => f(a).map ( aa => aa :: bb)))
  }

  // book solution
  def traverse2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(Nil))((a,b) => map2(f(a), b)(_ :: _))
  }




}
