package specs.unsafe

import org.scalacheck.Prop._

import sjsonnew.shaded.scalajson.ast._
import Generators._
import specs.{UTestScalaCheck, Utils}
import utest._

import scala.scalajs.js
import js.JSConverters._

object JArray extends TestSuite with UTestScalaCheck {

  val tests = TestSuite {
    "The unsafe.JArray value should" - {
      "convert toStandard" - toStandard
    }
  }

  def toStandard =
    forAll { (jArray: sjsonnew.shaded.scalajson.ast.unsafe.JArray) =>
      val values = jArray.value.map(_.toStandard).toVector
      jArray.toStandard == sjsonnew.shaded.scalajson.ast.JArray(values)
    }.checkUTest()
}
