package scalaz

sealed abstract class EventOrder[A] extends Product with Serializable {
  final def map[B](f: A => B): EventOrder[B] =
    this match {
      case LeftFirst() => LeftFirst()
      case RightFirst() => RightFirst()
      case Simultaneous(a) => Simultaneous(f(a))
    }
}

final case class LeftFirst[A]() extends EventOrder[A]
final case class RightFirst[A]() extends EventOrder[A]
final case class Simultaneous[A](a: A) extends EventOrder[A]

object EventOrder {
  implicit val instance: Functor[EventOrder] =
    new Functor[EventOrder] {
      def map[A, B](fa: EventOrder[A])(f: A => B) =
        fa map f
    }
}
