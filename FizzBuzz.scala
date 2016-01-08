def FizzBuzz(n: Int): String = {
  go(n: Int): String = 
    n match {
      case n%15 == 0 => FizzBuzz
      case n%3 == 0 => Fizz
      case n%5 == 0 => Buzz
      case _ => n
    }  
  go(n)
}
