package specs

import org.scalacheck.Prop._
import utest._

import sjsonnew.shaded.scalajson.ast._
import Generators._

import scala.scalajs.js
import js.JSConverters._

object JObject extends TestSuite with UTestScalaCheck {

  val tests = TestSuite {
    "The JObject value should" - {
      "convert toUnsafe" - toUnsafe
      "equals" - testEquals
    }
  }

  def toUnsafe =
    forAll { (jObject: sjsonnew.shaded.scalajson.ast.JObject) =>
      val values = jObject.value.map {
        case (k, v) =>
          sjsonnew.shaded.scalajson.ast.unsafe.JField(k, v.toUnsafe)
      }
      Utils.unsafeJValueEquals(jObject.toUnsafe,
                               sjsonnew.shaded.scalajson.ast.unsafe.JObject(values.toJSArray))
    }.checkUTest()

  def testEquals =
    forAll { (jObject: sjsonnew.shaded.scalajson.ast.JObject) =>
      sjsonnew.shaded.scalajson.ast.JObject(jObject.value) == sjsonnew.shaded.scalajson.ast.JObject(
        jObject.value)
    }.checkUTest()
}
