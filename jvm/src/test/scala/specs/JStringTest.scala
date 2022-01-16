package specs

import sjsonnew.shaded.scalajson.ast.JString

class JStringTest extends Spec {
  def is =
    s2"""
  The JString value should
    read a String $readStringJString
    convert toUnsafe $toUnsafe
    equals $testEquals
  """

  def readStringJString = prop { (s: String) =>
    JString(s).value must beEqualTo(s)
  }

  def toUnsafe = prop { (b: Boolean) =>
    sjsonnew.shaded.scalajson.ast.JBoolean(b).toUnsafe == sjsonnew.shaded.scalajson.ast.unsafe.JBoolean(b)
  }

  def testEquals = prop { (s: String) =>
    sjsonnew.shaded.scalajson.ast.JString(s) == sjsonnew.shaded.scalajson.ast.JString(s)
  }
}
