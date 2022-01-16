package specs

import Generators._

class JValue extends Spec {
  def is =
    s2"""
  The JArray value should
   equals $testEquals
  """

  def testEquals = prop { (jValue: sjsonnew.shaded.scalajson.ast.JValue) =>
    // Is there a better way to do this?
    val cloned = jValue match {
      case sjsonnew.shaded.scalajson.ast.JNull => sjsonnew.shaded.scalajson.ast.JNull
      case jNumber: sjsonnew.shaded.scalajson.ast.JNumber =>
        sjsonnew.shaded.scalajson.ast.JNumber.fromString(jNumber.value).get
      case jString: sjsonnew.shaded.scalajson.ast.JString =>
        sjsonnew.shaded.scalajson.ast.JString(jString.value)
      case jArray: sjsonnew.shaded.scalajson.ast.JArray => sjsonnew.shaded.scalajson.ast.JArray(jArray.value)
      case jObject: sjsonnew.shaded.scalajson.ast.JObject =>
        sjsonnew.shaded.scalajson.ast.JObject(jObject.value)
      case jBoolean: sjsonnew.shaded.scalajson.ast.JBoolean =>
        sjsonnew.shaded.scalajson.ast.JBoolean(jBoolean.get)
    }
    jValue must beEqualTo(cloned)
  }
}
