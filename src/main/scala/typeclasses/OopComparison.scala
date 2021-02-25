package typeclasses

object TypeclassTask {

  trait HashCode[T] {
    def hash(entity: T): Int
  }

  object HashCode {
    def apply[F](implicit instance: HashCode[F]): HashCode[F] = instance
  }

  final implicit class HashCodeSyntax[A](x: A) {
    def hash(implicit h: HashCode[A]): Int = h.hash(x)
  }

  implicit val stringHashCode: HashCode[String] = _.hashCode

  println("abc".hash)
}
