package specs

import scala.json.ast.{JBoolean, JFalse, JTrue}

class JBoolean extends Spec {
  def is =
    s2"""
  The JBoolean value should
   read a Boolean $readBooleanJBoolean
   pattern match with JTrue $readBooleanJBooleanPatternMatchJTrue
   pattern match with JTrue and fail with scala.MatchError $readBooleanJBooleanPatternMatchJTrueFail
   pattern match with JFalse $readBooleanJBooleanPatternMatchJFalse
   pattern match with JFalse and fail with scala.MatchError $readBooleanJBooleanPatternMatchJFalseFail
   pattern match with JBoolean as true $readBooleanJBooleanPatternMatchJBooleanTrue
   pattern match with JBoolean as true and fail with scala.MatchError $readBooleanJBooleanPatternMatchJBooleanTrueFail
   pattern match with JBoolean as false $readBooleanJBooleanPatternMatchJBooleanFalse
   pattern match with JBoolean as false and fail with scala.MatchError $readBooleanJBooleanPatternMatchJBooleanFalseFail

  The JTrue value should
    read a Boolean as true $readBooleanJTrue

  The JFalse value should
    read a Boolean as false $readBooleanJFalse

  convert toUnsafe $toUnsafe
  equals $testEquals
  """

  def readBooleanJBoolean = prop { b: Boolean =>
    JBoolean(b).get must beEqualTo(b)
  }

  def readBooleanJBooleanPatternMatchJBooleanTrue = prop { b: Boolean =>
    {
      b == true
    } ==> {
      val result = JBoolean(true) match {
        case f @ JBoolean(true) => f
      }
      result.get must beEqualTo(b)
    }
  }

  def readBooleanJBooleanPatternMatchJBooleanTrueFail = {
    {
      JBoolean(true) match {
        case f @ JBoolean(false) => f
      }
    } must throwA[MatchError]
  }

  def readBooleanJBooleanPatternMatchJBooleanFalse = prop { b: Boolean =>
    {
      b == false
    } ==> {
      val result = JBoolean(b) match {
        case f @ JBoolean(false) => f
      }
      result.get must beEqualTo(b)
    }
  }

  def readBooleanJBooleanPatternMatchJBooleanFalseFail = {
    {
      JBoolean(false) match {
        case f @ JBoolean(true) => f
      }
    } must throwAn[MatchError]
  }

  def readBooleanJBooleanPatternMatchJTrue = prop { b: Boolean =>
    (b == true) ==> {
      val result = JBoolean(b) match {
        case f @ JTrue => f
      }
      result.get must beEqualTo(b)
    }
  }

  def readBooleanJBooleanPatternMatchJTrueFail = {
    {
      JBoolean(true) match {
        case f @ JFalse => f
      }
    } must throwAn[MatchError]
  }

  def readBooleanJBooleanPatternMatchJFalse = prop { b: Boolean =>
    (b == false) ==> {
      val result = JBoolean(b) match {
        case f @ JFalse => f
      }
      result.get must beEqualTo(b)
    }
  }

  def readBooleanJBooleanPatternMatchJFalseFail = {
    {
      JBoolean(false) match {
        case f @ JTrue => f
      }
    } must throwAn[MatchError]
  }

  def readBooleanJTrue = prop { b: Boolean =>
    (b == true) ==> {
      JTrue.get must beEqualTo(b)
    }
  }

  def readBooleanJFalse = prop { b: Boolean =>
    (b == false) ==> {
      JFalse.get must beEqualTo(b)
    }
  }

  def testEquals = prop { b: Boolean =>
    scala.json.ast.JBoolean(b) must beEqualTo(scala.json.ast.JBoolean(b))
  }

  def toUnsafe = prop { b: Boolean =>
    scala.json.ast.JBoolean(b).toUnsafe must beEqualTo(
        scala.json.ast.unsafe.JBoolean(b))
  }
}
