package specs

import Generators._

class JObject extends Spec {
  def is =
    s2"""
  The JArray value should
   convert toUnsafe $toUnsafe
   equals $testEquals
  """

  def toUnsafe = prop { jObject: sjsonnew.shaded.scalajson.ast.JObject =>
    val values = jObject.value.map {
      case (k, v) =>
        sjsonnew.shaded.scalajson.ast.unsafe.JField(k, v.toUnsafe)
    }
    Utils.unsafeJValueEquals(jObject.toUnsafe,
                             sjsonnew.shaded.scalajson.ast.unsafe.JObject(values.toArray))
  }

  def testEquals = prop { jObject: sjsonnew.shaded.scalajson.ast.JObject =>
    sjsonnew.shaded.scalajson.ast.JObject(jObject.value) must beEqualTo(
      sjsonnew.shaded.scalajson.ast.JObject(jObject.value))
  }
}
