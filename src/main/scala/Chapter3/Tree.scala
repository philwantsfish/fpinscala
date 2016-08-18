package Chapter3


sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(v) => 1
    case Branch(left, right) => size(left) + size(right) + 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(v) => 1
    case Branch(left, right) => (depth(left) max depth(right)) + 1
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    def loop(t: Tree[A]): Tree[B] = {
      t match {
        case Leaf(v) => Leaf(f(v))
        case Branch(left, right) => Branch(loop(left), loop(right))
      }
    }
    loop(t)
  }

  def foldRight[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(left, right) => g(foldRight(left)(f)(g), foldRight(right)(f)(g))
  }

  def sizeViaFoldRight[A](t: Tree[A]): Int = {
    foldRight(t)(a => 1)((a,b) => 1+a+b)
  }

  def maximumViaFoldRight(t: Tree[Int]): Int = {
    foldRight(t)(a => a)((a,b) => a max b)
  }

  def depthViaFoldRight[A](t: Tree[A]): Int = {
    foldRight(t)(a => 1)((a,b) => (a max b) + 1)
  }
}
