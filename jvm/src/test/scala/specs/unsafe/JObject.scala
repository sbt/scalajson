package specs.unsafe

import specs.Spec
import sjsonnew.shaded.scalajson.ast.unsafe._
import Generators._

class JObject extends Spec {
  def is =
    s2"""
  The unsafe.JArray value should
    convert toStandard $toStandard
  """

  def toStandard = prop { jObject: sjsonnew.shaded.scalajson.ast.unsafe.JObject =>
    val values = jObject.value.map { x =>
      (x.field, x.value.toStandard)
    }.toMap
    jObject.toStandard == sjsonnew.shaded.scalajson.ast.JObject(values)
  }

}
