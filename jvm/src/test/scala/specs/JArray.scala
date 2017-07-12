package specs

import Generators._

/**
  * Created by matthewdedetrich on 5/05/2016.
  */
class JArray extends Spec {
  def is =
    s2"""
  The JArray value should
   convert toUnsafe $toUnsafe
   equals $testEquals
  """

  def toUnsafe = prop { jArray: sjsonnew.shaded.scalajson.ast.JArray =>
    val values = jArray.value.map(_.toUnsafe).toArray

    Utils.unsafeJValueEquals(
      jArray.toUnsafe,
      sjsonnew.shaded.scalajson.ast.unsafe.JArray(values)
    ) must beTrue
  }

  def testEquals = prop { jArray: sjsonnew.shaded.scalajson.ast.JArray =>
    sjsonnew.shaded.scalajson.ast.JArray(jArray.value) must beEqualTo(
      sjsonnew.shaded.scalajson.ast.JArray(jArray.value))
  }
}
