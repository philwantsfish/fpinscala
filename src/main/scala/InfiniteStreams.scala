
object InfiniteStreams {

  def fib: Stream[Int] = fibFrom(0, 1)
  def fibFrom(a: Int, b: Int): Stream[Int] = a #:: fibFrom(b, a + b)

  /**
   * A function to help generate infinite streams. unfold take an initial state and a function
   * that generates an element and the next state.
   *
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => a #:: unfold(s)(f)
      case None => Stream.empty
    }
  }

  /**
   * Sieve of Eratosthenes modeled as a infinite stream of primes
   *
   * "It does so by iteratively marking as composite (i.e., not prime) the multiples of
   * each prime, starting with the multiples of 2" - Wikipedia
   *
   * The algorithm in 3 steps:
   * ---
   * 1) Evaluate each number sequentially using a set of functions that check for primeness
   * 2) When finding a prime, convert the prime number into function and add that to the list
   * 3) The algorithm starts at number 2 with an empty set of functions
   * ---
   *
   * Using unfold the state required is the list prime check functions
   */
  def primes: Stream[Int] = {
    val checkPrimeFunctions: Seq[Int => Boolean] = Seq()
    val makeCheckPrimeFunction: Int => Int => Boolean = a => b => b % a == 0

    def isComposite(fs: Seq[Int => Boolean], num: Int): Boolean = fs.exists { f => f(num) }

    1 #:: unfold((2, checkPrimeFunctions)) {
      case (i, fs) =>
        def go(num: Int, ffs: Seq[Int => Boolean]): (Int, (Int, Seq[Int=>Boolean])) = {
          if(isComposite(fs, num)) go(num+1, ffs)
          else (num, (num+1, ffs :+ makeCheckPrimeFunction(num)))
        }
        Some(go(i, fs))
    }
  }
}
