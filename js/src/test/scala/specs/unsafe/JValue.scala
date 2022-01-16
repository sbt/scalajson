package specs.unsafe

import org.scalacheck.Prop._
import specs.UTestScalaCheck
import utest._

import Generators._

object JValue extends TestSuite with UTestScalaCheck {
  val tests = TestSuite {
    "The JValue value should" - {
      "equals" - testEquals
    }
  }

  def testEquals =
    forAll { (jValue: sjsonnew.shaded.scalajson.ast.unsafe.JValue) =>
      // Is there a better way to do this?
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
      jValue == cloned
    }.checkUTest()
}
