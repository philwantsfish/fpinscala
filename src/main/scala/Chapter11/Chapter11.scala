package Chapter11

import Chapter4._

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  // Aka unzip
  // Call map on the tuple, then take the left side. Repeat for the right
  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
  (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] =
    e match {
      case Left(fa) => map(fa)(Left(_))
      case Right(fb) => map(fb)(Right(_))
    }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[F[_]] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

  def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] = {
    lma match {
      case a :: as => flatMap(a) { b => map(sequence(as)) { c => b :: c }  }
      case Nil => unit(Nil)
    }
  }

  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] = {
    val lbs = la.map { a => f(a) }
    sequence(lbs)
  }

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = {
    a => flatMap(f(a))(g)
  }

  // Never would've got this one. Took it from the answers. Wow.
  def flatMapWithCompose[A,B](ma: F[A])(f: A => F[B]): F[B] = {
    compose( (_: Unit) => ma, f)(())
  }

  def join[A](mma: F[F[A]]): F[A] = {
    flatMap(mma)(a => a)
  }

  def flatMapWithJoinAndMap[A,B](ma: F[A])(f: A => F[B]): F[B] = {
    join(map(ma)(f))
  }
}


object Monad {
  val optionMonad = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] = ma match {
      case Some(a) => f(a)
      case None => None
    }
  }

  val idMonad = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)
    override def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]): Id[B] = f(ma.values)
  }
}

case class Id[A](values: A) {
  def map[B](f: A => B): Id[B] = Id(f(values))
  def flatMap[B](f: A => Id[B]): Id[B] = f(values)
}





