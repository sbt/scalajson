package specs

import utest._

object JNull extends TestSuite with UTestScalaCheck {

  val tests = TestSuite {
    "The JNull value should" - {
      "convert to jsAny" - toJsAny
      "convert toUnsafe" - toUnsafe
      "equals" - testEquals
    }
  }

  def toJsAny = {
    sjsonnew.shaded.scalajson.ast.JNull.toJsAny == null
  }

  def toUnsafe = {
    sjsonnew.shaded.scalajson.ast.JNull.toUnsafe == sjsonnew.shaded.scalajson.ast.unsafe.JNull
  }

  def testEquals = {
    sjsonnew.shaded.scalajson.ast.JNull == sjsonnew.shaded.scalajson.ast.JNull
  }
}
