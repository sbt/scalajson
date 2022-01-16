package testpkg

import sjsonnew.shaded.scalajson.ast._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object JNumberTest extends Properties("JNumber") {
  property("readLongJNumber") = forAll { (l: Long) =>
    JNumber(l).value == l.toString
  }

  property("readBigDecimalJNumber") = forAll { (b: BigDecimal) =>
    JNumber(b).value == b.toString()
  }

  property("readBigIntJNumber") = forAll { (b: BigInt) =>
    JNumber(b).value == b.toString
  }

  property("readIntJNumber") = forAll { (i: Int) =>
    JNumber(i).value == i.toString
  }

  property("readDoubleJNumber") = forAll { (d: Double) =>
    JNumber(d) match {
      case JNull => JNull == JNull
      case JNumber(value) => value == d.toString
    }
  }

  property("readDoubleNANJNumber") = {
    JNumber(Double.NaN) match {
      case JNull => true
      case _ => false
    }
  }

  property("readDoublePositiveInfinityJNumber") = {
    JNumber(Double.PositiveInfinity) match {
      case JNull => true
      case _ => false
    }
  }

  property("readDoubleNegativeInfinityJNumber") = {
    JNumber(Double.NegativeInfinity) match {
      case JNull => true
      case _ => false
    }
  }

  property("readFloatJNumber") = forAll { (f: Float) =>
    JNumber(f) match {
      case JNull => JNull == JNull
      case JNumber(value) => value == f.toString
    }
  }

  property("readFloatNANJNumber") = {
    JNumber(Float.NaN) match {
      case JNull => true
      case _ => false
    }
  }

  property("readFloatPositiveInfinityJNumber") = {
    JNumber(Float.PositiveInfinity) match {
      case JNull => true
      case _ => false
    }
  }

  property("readFloatNegativeInfinityJNumber") = {
    JNumber(Float.NegativeInfinity) match {
      case JNull => true
      case _ => false
    }
  }

  property("readShortJNumber") = forAll { (s: Short) =>
    JNumber(s).value == s.toString
  }

  property("readCharArrayJNumber") = {
    JNumber("34".toCharArray).get.## ==
      JNumber.fromString("34").get.##
  }

  property("hashCodeEqualsDecimal") = {
    JNumber.fromString("34").get.## ==
      JNumber.fromString("34.0").get.##
  }

  property("hashCodeEqualsDecimal2") = {
    JNumber.fromString("34").get.## ==
      JNumber.fromString("34.00").get.##
  }

  property("hashCodeNotEqualsDecimal") = {
    JNumber.fromString("34").get.## != JNumber
      .fromString("34.01")
      .get
      .##
  }

  property("hashCodeNotEqualsDecimal2") = {
    JNumber.fromString("34").get.## != JNumber
      .fromString("34.001")
      .get
      .##
  }

  property("hashCodeEqualsE") = {
    JNumber.fromString("34e34").get.## ==
      JNumber.fromString("34e034").get.##
  }

  property("hashCodeEqualsE2") = {
    JNumber.fromString("34e34").get.## ==
      JNumber.fromString("34e0034").get.##
  }

  property("hashCodeEqualsENegative") = {
    JNumber.fromString("34e-0").get.## ==
      JNumber.fromString("34").get.##
  }

  property("hashCodeEqualsENegative2") = {
    JNumber.fromString("34e-00").get.## ==
      JNumber.fromString("34").get.##
  }

  property("hashCodeNotEqualsENegative") = {
    JNumber.fromString("34e-01").get.## != JNumber
      .fromString("34")
      .get
      .##
  }

  property("hashCodeNotEqualsENegative2") = {
    JNumber.fromString("34e-001").get.## != JNumber
      .fromString("34")
      .get
      .##
  }

  property("hashCodeEqualsEPositive") = {
    JNumber.fromString("34e+0").get.## ==
      JNumber.fromString("34").get.##
  }

  property("hashCodeEqualsEPositive2") = {
    JNumber.fromString("34e+00").get.## ==
      JNumber.fromString("34").get.##
  }

  property("hashCodeNotEqualsEPositive") = {
    JNumber.fromString("34e+01").get.## != JNumber
      .fromString("34")
      .get
      .##
  }

  property("hashCodeNotEqualsEPositive2") = {
    JNumber.fromString("34e+001").get.## != JNumber
      .fromString("34")
      .get
      .##
  }

  property("toUnsafe") = forAll { (b: BigDecimal) =>
    sjsonnew.shaded.scalajson.ast.JNumber(b).toUnsafe ==
      sjsonnew.shaded.scalajson.ast.unsafe.JNumber(b)
  }

  property("testEquals") = forAll { (b: BigDecimal) =>
    sjsonnew.shaded.scalajson.ast.JNumber(b) ==
      sjsonnew.shaded.scalajson.ast.JNumber(b)
  }

  property("testCopy") = forAll { (b1: BigDecimal, b2: BigDecimal) =>
    val asString = b2.toString()
    sjsonnew.shaded.scalajson.ast.JNumber(b1).copy(value = asString) ==
      sjsonnew.shaded.scalajson.ast.JNumber(b2)
  }

  property("testCopyFail") = forAll { (b: BigDecimal) =>
    try {
      sjsonnew.shaded.scalajson.ast.JNumber(b).copy(value = "not a number")
      true
    } catch {
      case e: NumberFormatException => e.getMessage == "not a number"
    }
  }
}
