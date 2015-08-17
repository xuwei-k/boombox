package scalaz

import scalaz.Machina._

sealed abstract class Machina[W[_], M[_], A, B] extends Product with Serializable {

  final def >->[C](that: Machina[W, M, B, C])(implicit W: Functor[W], M: Functor[M]): Machina[W, M, A, C] =
    (this, that) match {
      case (Yield(a, w), Await(f)) =>
        Past(W.map(w)(_ >-> f(a)))
      case (Await(f), k) =>
        Await(f.andThen(_ >-> k))
      case (k, Yield(a, w)) =>
        Yield(a, W.map(w)(k >-> _))
      case (Past(m), n) =>
        Past(W.map(m)(_ >-> n))
      case (m, Past(n)) =>
        Past(W.map(n)(m >-> _))
      case (Future(m), n) =>
        Future(M.map(m)(_ >-> n))
      case (m, Future(n)) =>
        Future(M.map(n)(m >-> _))
    }

  final def map[C](f: B => C)(implicit W: Functor[W], M: Functor[M]): Machina[W, M, A, C] =
    this match {
      case Yield(b, run) =>
        Yield(f(b), W.map(run)(_.map(f)))
      case Await(run) =>
        Await(run.andThen(_.map(f)))
      case Past(run) =>
        Past(W.map(run)(_.map(f)))
      case Future(run) =>
        Future(M.map(run)(_.map(f)))
    }

}

object Machina {

  final case class Yield[W[_], M[_], A, B](b: B, run: W[Machina[W, M, A, B]]) extends Machina[W, M, A, B]

  final case class Await[W[_], M[_], A, B](run: A => Machina[W, M, A, B]) extends Machina[W, M, A, B]

  final case class Past[W[_], M[_], A, B](run: W[Machina[W, M, A, B]]) extends Machina[W, M, A, B]

  final case class Future[W[_], M[_], A, B](run: M[Machina[W, M, A, B]]) extends Machina[W, M, A, B]

  implicit def instance[W[_]: Functor, M[_]: Functor, A]: Functor[({type l[a] = Machina[W, M, A, a]})#l] =
    new Functor[({type l[a] = Machina[W, M, A, a]})#l] {
      def map[C, D](fa: Machina[W, M, A, C])(f: C => D) =
        fa map f
    }

}
