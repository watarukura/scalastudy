class Cafe {
  def buyCoffee(cc: CreditCard): Coffee = {
    val cup = new Coffee()
    cc.charge(cup.price)
    cup
  }
}

class Coffee {
  val price = 100
}

class CreditCard {
  def charge(price) = {}
}
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

  def fib(n: Int): Int ={
    def loop(n: Int, prev: Int, cur: Int) =
      if (n == 0) prev
      else loop(n - 1, cur, prev + cur)
    loop(n, 0, 1)
  }

  def fib_pmatch(n: Int): Int = {
    def go(n: Int): Int =
      n match {
        case 0 => 0
        case 1 => 1
        case _ => go(n - 1) + go(n - 2)
      }​
    go(n)
  }

  def main(args: Array[String]): Unit =
    println(formatAbs(-42))
  println(formatFactorial(20))
  println(fib(8))
  println(fib_pmatch(2))
}