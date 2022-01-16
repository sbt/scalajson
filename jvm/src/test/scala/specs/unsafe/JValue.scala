package specs.unsafe

import specs.Spec
import sjsonnew.shaded.scalajson.ast.unsafe._
import Generators._

class JValue extends Spec {
  def is =
    s2"""
  The JValue value should
   equals $testEquals
  """

  def testEquals = prop { (jValue: sjsonnew.shaded.scalajson.ast.unsafe.JValue) =>
    val cloned = jValue match {
      case sjsonnew.shaded.scalajson.ast.unsafe.JNull => sjsonnew.shaded.scalajson.ast.unsafe.JNull
      case jNumber: sjsonnew.shaded.scalajson.ast.unsafe.JNumber =>
        sjsonnew.shaded.scalajson.ast.unsafe.JNumber(jNumber.value)
      case jString: sjsonnew.shaded.scalajson.ast.unsafe.JString =>
        sjsonnew.shaded.scalajson.ast.unsafe.JString(jString.value)
      case jArray: sjsonnew.shaded.scalajson.ast.unsafe.JArray =>
        sjsonnew.shaded.scalajson.ast.unsafe.JArray(jArray.value)
      case jObject: sjsonnew.shaded.scalajson.ast.unsafe.JObject =>
        sjsonnew.shaded.scalajson.ast.unsafe.JObject(jObject.value)
      case jBoolean: sjsonnew.shaded.scalajson.ast.unsafe.JBoolean =>
        sjsonnew.shaded.scalajson.ast.unsafe.JBoolean(jBoolean.get)
    }
    jValue must beEqualTo(cloned)
  }
}
