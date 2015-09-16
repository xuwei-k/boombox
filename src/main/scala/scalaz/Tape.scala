package scalaz

import scalaz.Tape._

sealed abstract class Tape[W[_], M[_], A] extends Product with Serializable {
  final def map[B](f: A => B)(implicit W: Functor[W], M: Functor[M]): Tape[W, M, B] =
    this match {
      case Yield(a, w) =>
        Yield(f(a), W.map(w)(_.map(f)))
      case Effect(m) =>
        Effect(M.map(m)(_.map(f)))
    }
}

object Tape {
  private final case class Yield[W[_], M[_], A](a: A, w: W[Tape[W, M, A]]) extends Tape[W, M, A]
  private final case class Effect[W[_], M[_], A](m: M[Tape[W, M, A]]) extends Tape[W, M, A]
}

