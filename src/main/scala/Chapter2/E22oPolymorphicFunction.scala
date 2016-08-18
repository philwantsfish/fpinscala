package Chapter2


object E22oPolymorphicFunction {
  // im guessing this solution is cheating, we probably shouldn't know about sliding and forall yet
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    as.sliding(2).forall { a => ordered(a(0), a(1)) }
  }

  def isSorted2[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(arr: Array[A], index: Int): Boolean = {
      if (index >= arr.length - 1) true
      else if (!ordered(arr(index), arr(index+1))) false
      else loop(arr, index+1)
    }

    loop(as, 0)
  }
}
