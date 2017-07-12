package specs

import org.scalacheck.Prop._
import utest._

import sjsonnew.shaded.scalajson.ast._
import Generators._

import scala.scalajs.js

object JValue extends TestSuite with UTestScalaCheck {

  val tests = TestSuite {
    "The JString value should" - {
      "equals" - testEquals
    }
  }

  def testEquals =
    forAll { jValue: sjsonnew.shaded.scalajson.ast.JValue =>
      // Is there a better way to do this?
      val cloned = jValue match {
        case sjsonnew.shaded.scalajson.ast.JNull => sjsonnew.shaded.scalajson.ast.JNull
        case jNumber: sjsonnew.shaded.scalajson.ast.JNumber =>
          sjsonnew.shaded.scalajson.ast.JNumber.fromString(jNumber.value).get
        case jString: sjsonnew.shaded.scalajson.ast.JString =>
          sjsonnew.shaded.scalajson.ast.JString(jString.value)
        case jArray: sjsonnew.shaded.scalajson.ast.JArray =>
          sjsonnew.shaded.scalajson.ast.JArray(jArray.value)
        case jObject: sjsonnew.shaded.scalajson.ast.JObject =>
          sjsonnew.shaded.scalajson.ast.JObject(jObject.value)
        case jBoolean: sjsonnew.shaded.scalajson.ast.JBoolean =>
          sjsonnew.shaded.scalajson.ast.JBoolean(jBoolean.get)
      }
      jValue == cloned
    }.checkUTest()
}
