object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int): String = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)

    go(n, 1)
  }

  private def formatFactorial(x: Int): String = {
    val msg = "The factorial of %d is %d"
    msg.format(x, factorial(x))
  }

  def fib(n: Int): Int = {
    def loop(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else loop(n - 1, cur, prev + cur)
    loop(n, 0, 1)
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  // Exercise 2: Implement a polymorphic function to check whether
  // an `Array[A]` is sorted
  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean =
      if (n >= as.length-1) true
      else if (gt(as(n), as(n+1))) false
      else go(n+1)

    go(0)
  }

  def curry[A,B,C](f: (A,B) => C): A => (B => C) =
    a => b => f(a,b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a,b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a) => f(g(a))

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(20))
    println(fib(8))
  }
}
