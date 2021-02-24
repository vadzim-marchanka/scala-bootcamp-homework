package typeclasses

object Task1 {
  final case class Money(amount: BigDecimal)

  implicit val moneyOrdering: Ordering[Money] = Ordering.by(_.amount)
}

object Task2 extends App {
  trait Show[T] {
    def show(entity: T): String
  }

  final case class User(id: String, name: String)

  implicit class ShowSyntax[A](x: A) {
    def show(implicit s: Show[A]): String = s.show(x)
  }

  implicit val userShow: Show[User] = _.toString

  println(User("1", "Oleg").show)
}

object Task3 extends App {
  type Error = String
  trait Parse[T] { // invent any format you want or it can be csv string
    def parse(entity: String): Either[Error, T]
  }

  final case class User(id: String, name: String)

  implicit val userParser: Parse[User] = str => {
    str.split("\\|").toList match {
      case id :: name :: Nil => Right(User(id, name))
      case k => Left(s"Parse failure at $k . Can not parse the string $str . The string should be represented as id%user.")
    }
  }

  final implicit class ParseSyntax(x: String) {
    def parse[A](implicit p: Parse[A]): Either[Error, A] = p.parse(x)
  }

  println("lalala".parse[User])
  println("1312|Nick".parse[User])
}

object Task4 extends App {

  trait Equals[T] {
    def equals(a: T, b: T): Boolean
  }

  object Equals {
    def default[T](a: T, b: T): Boolean = a.equals(b)
  }

  final implicit class EqualsSyntax[T](a: T) {
    def ===(b: T)(implicit e: Equals[T]): Boolean = e.equals(a, b)
  }

  implicit val strEquals: Equals[String] = Equals.default
  implicit val intEquals: Equals[Int] = Equals.default

  println("asdf" === "asdf")
  println("asdf" === "asd")
  //println(123 === "asdf") does not compile
}

object AdvancedHomework extends App {

  trait FlatMap[F[_]] {
    def customFlatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  }

  implicit class FlatMapSyntax[F[_], A](x: F[A]) {
    def customFlatMap[B](f: A => F[B])(implicit m: FlatMap[F]): F[B] = m.customFlatMap(x)(f)
  }

  implicit val optionMap: FlatMap[Option] = new FlatMap[Option] {
    override def customFlatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
  }

  println(Option(1).customFlatMap(el => Option(el + 2)))

}
