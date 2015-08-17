package scalaz

import scala.util.Random
import scalaprops.Gen

object FunctionEqual extends FunctionEqual(30)

sealed class FunctionEqual(size: Int) {
  implicit def f1[A1: Gen, B](implicit B: Equal[B]): Equal[A1 => B] = {
    val values = Gen[IList[A1]].sample(size = size, seed = Random.nextLong())

    Equal.equal((x, y) =>
      Foldable[IList].all(values)(a => B.equal(x(a), y(a)))
    )
  }

  implicit def f2[A1: Gen, A2: Gen, B](implicit B: Equal[B]): Equal[(A1, A2) => B] =
    f1[(A1, A2), B].contramap(_.tupled)
}

