sealed trait Either[+E, +A] {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

  def SafeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }

  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(b) => Right(b)
    case Left(e) => Left(e)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A,B) => C): Either[EE, C] =
    for {a <- this; b1 <- b} yield f(a, b1)

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil => Right(Nil)
    case h::t => h flatMap ( hh => sequence(t) map (hh::_))
  }

//  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
//    case Nil => Right(Nil)
//    case h::t => (f(h) map2 traverse(t)(f) (hh::_))
//  }

  case class Person( name: Name, age: Age)
  sealed class Name( val value: String)
  sealed class Age( val value: Int)

  def mkName( name: String): Either[ String, Name] =
    if (name == "" || name == null) Left(" Name is empty.")
    else Right( new Name( name))

  def mkAge( age: Int): Either[ String, Age] =
    if (age < 0) Left(" Age is out of range.")
    else Right( new Age( age))

  def mkPerson( name: String, age: Int): Either[ String, Person] =
    mkName(name). map2(mkAge(age))(Person(_, _))

  def main(args: Array[String]): Unit = {
    val person = mkPerson("aaa", 100)
    println(person)
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]
