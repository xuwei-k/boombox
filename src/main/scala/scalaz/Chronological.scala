package scalaz

trait Chronological[F[_]] extends Comonad[F] {
  def coincidence[A, B](fa: F[A], fb: F[B]): EventOrder[F[(A, B)]]
}

object Chronological {
  def apply[F[_]](implicit F: Chronological[F]): Chronological[F] = F

  implicit def chronologicalTuple[I](implicit I: Order[I]): Chronological[({type l[a] = (I, a)})#l] =
    new Chronological[({type l[a] = (I, a)})#l] {
      val F: Comonad[({type l[a] = (I, a)})#l] = scalaz.std.tuple.tuple2Instance

      override def coincidence[A, B](fa: (I, A), fb: (I, B)): EventOrder[(I, (A, B))] =
        I.order(fa._1, fb._1) match {
          case Ordering.EQ => Simultaneous((fa._1, (fa._2, fb._2)))
          case Ordering.LT => LeftFirst()
          case Ordering.GT => RightFirst()
        }

      override def copoint[A](p: (I, A)) =
        F.copoint(p)

      override def cobind[A, B](fa: (I, A))(f: ((I, A)) => B) =
        F.cobind(fa)(f)

      override def map[A, B](fa: (I, A))(f: A => B) =
        F.map(fa)(f)
    }
}
