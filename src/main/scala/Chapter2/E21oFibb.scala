package Chapter2



object E21oFibb {
  /**
   * index: 0 1 2 3 4 5 6
   * fib n: 0 1 1 2 3 5 8
   */
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(iter: Int, x: Int, y: Int): Int = {
      if (iter == n) x + y
      else loop(iter+1, y, x+y)
    }

    if(n < 0) throw new RuntimeException("You dun goofed")
    else if(n < 2) n
    else loop(2, 0, 1)
  }

  def fib2(n: Int): Int = {
    @annotation.tailrec
    def loop(iter: Int, x: Int, y: Int): Int = {
      if (iter == n) x
      else loop(iter+1, y, x+y)
    }

    if(n < 0) throw new RuntimeException("You dun goofed")
    else loop(0, 0, 1)
  }
}
