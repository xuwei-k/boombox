package scalaz

import scalaprops.{Cogen, Gen}
import scalaz.std.tuple._

final case class Needle[I, A](i: I, f: I => A, a: A) {
  def map[B](g: A => B): Needle[I, B] =
    Needle(i, f.andThen(g), g(a))
}

object Needle {

  implicit def needleCogen[I: Cogen, A: Cogen](implicit F: Cogen[I => A]): Cogen[Needle[I, A]] =
    Cogen.from3(unapply)

  implicit def needleGen[I: Gen, A: Gen](implicit F: Gen[I => A]): Gen[Needle[I, A]] =
    Gen.from3(apply)

  implicit def needleEqual[I: Equal, A: Equal](implicit F: Equal[I => A]): Equal[Needle[I, A]] =
    Equal.equalBy(Needle.unapply(_).get)

  implicit def instance[I]: Comonad[({type l[a] = Needle[I, a]})#l] =
    new Comonad[({type l[a] = Needle[I, a]})#l] {
      def map[A, B](fa: Needle[I, A])(f: A => B) =
        fa map f

      override def copoint[A](p: Needle[I, A]): A =
        p.a

      override def cojoin[A](s: Needle[I, A]) =
        Needle(s.i, j => Needle(j, s.f, s.a), s)

      override def cobind[A, B](fa: Needle[I, A])(f: Needle[I, A] => B) =
        map(cojoin(fa))(f)
    }
}
