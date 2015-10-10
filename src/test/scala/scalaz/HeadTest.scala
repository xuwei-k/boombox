package scalaz

import scalaprops._
import FunctionEqual._
import scalaz.std.anyVal._

object HeadTest extends Scalaprops {

  val laws = scalazlaws.comonad.all[({type l[a] = Head[Byte, a]})#l]

}
