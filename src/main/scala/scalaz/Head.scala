package scalaz

import scalaprops.{Gen, Cogen}

/*
instance Ord i => Chronological (Head i) where
  coincidence (Head i f) (Head j g) = case compare i j of
    EQ -> Simultaneous (Head i (liftA2 (,) f g))
    LT -> LeftFirst
    GT -> RightFirst
 */

final case class Head[I, A](i: I, f: Maybe[I] => A) {
  def map[B](g: A => B): Head[I, B] =
    Head(i, f.andThen(g))
}

object Head {
  implicit def instance[I]: Comonad[({type l[a] = Head[I, a]})#l] =
    new Comonad[({type l[a] = Head[I, a]})#l] {
      override def copoint[A](p: Head[I, A]) =
        p.f(Maybe.empty)

      override def cobind[A, B](fa: Head[I, A])(k: Head[I, A] => B) =
        Head(fa.i, m => k(Head(m.getOrElse(fa.i), fa.f)))

      override def map[A, B](fa: Head[I, A])(f: A => B) =
        fa map f
    }

  implicit def headEqual[I: Equal, A](implicit A: Equal[Maybe[I] => A]): Equal[Head[I, A]] =
    Divide[Equal].deriving2(Function.unlift(Head.unapply))

  implicit def headGen[I: Gen: Cogen, A: Gen]: Gen[Head[I, A]] =
    Gen.from2(Head.apply)

  implicit def headCogen[I: Gen: Cogen, A: Cogen]: Cogen[Head[I, A]] =
    Cogen.from2(Head.unapply)
}
