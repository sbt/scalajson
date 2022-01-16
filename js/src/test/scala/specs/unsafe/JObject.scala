package specs.unsafe

import org.scalacheck.Prop._
import utest._

import sjsonnew.shaded.scalajson.ast._
import Generators._
import specs.{UTestScalaCheck, Utils}

import scala.scalajs.js
import js.JSConverters._

object JObject extends TestSuite with UTestScalaCheck {

  val tests = TestSuite {
    "The unsafe.JObject value should" - {
      "convert toStandard" - toStandard
    }
  }

  def toStandard =
    forAll { (jObject: sjsonnew.shaded.scalajson.ast.unsafe.JObject) =>
      val values = jObject.value.map { x =>
        (x.field, x.value.toStandard)
      }.toMap

      jObject.toStandard == sjsonnew.shaded.scalajson.ast.JObject(values)
    }.checkUTest()
}
