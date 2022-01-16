package specs.unsafe

import specs.Spec
import sjsonnew.shaded.scalajson.ast.unsafe._
import Generators._

class JArray extends Spec {
  def is =
    s2"""
  The unsafe.JArray value should
    convert toStandard $toStandard
  """

  def toStandard = prop { (jArray: sjsonnew.shaded.scalajson.ast.unsafe.JArray) =>
    val values = jArray.value.map(_.toStandard).toVector
    jArray.toStandard == sjsonnew.shaded.scalajson.ast.JArray(values)
  }
}
