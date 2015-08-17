package scalaz

import scalaprops._
import scalaz.std.anyVal._
import FunctionEqual._

object NeedleTest extends Scalaprops {

  type F[A] = Needle[Byte, A]

  val laws = scalazlaws.comonad.all[F]

}
