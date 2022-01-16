package specs

import org.scalacheck.Prop.forAll
import utest._

object JNumber extends TestSuite with UTestScalaCheck {

  val tests = TestSuite {
    "The JNumber value should" - {
      "read a Long" - readLongJNumber
      "read a BigDecimal" - readBigDecimalJNumber
      "read a Double" - readDoubleJNumber
      "read a BigInt" - readBigIntJNumber
      "read an Int" - readIntJNumber
      "read a Double NaN" - readDoubleNANJNumber
      "read a Double Positive Infinity" - readDoublePositiveInfinityJNumber
      "read a Double Negative Infinity" - readDoubleNegativeInfinityJNumber
      "read a Float" - readFloatJNumber
      "read a Float NaN" - readFloatNANJNumber
      "read a Float Positive Infinity" - readFloatPositiveInfinityJNumber
      "read a Float Negative Infinity" - readFloatNegativeInfinityJNumber
      "read a Short" - readShortJNumber
      "hashCode equals decimal" - hashCodeEqualsDecimal
      "hashCode equals decimal #2" - hashCodeEqualsDecimal2
      "hashCode not equals decimal" - hashCodeNotEqualsDecimal
      "hashCode not equals decimal #2" - hashCodeNotEqualsDecimal2
      "hashCode equals e" - hashCodeEqualsE
      "hashCode equals e #2" - hashCodeEqualsE2
      "convert to jsAny" - toJsAny
      "hashCode equals e negative" - hashCodeEqualsENegative
      "hashCode equals e negative" - hashCodeEqualsENegative2
      "hashCode not equals e negative" - hashCodeNotEqualsENegative
      "hashCode not equals e negative #2" - hashCodeNotEqualsENegative2
      "hashCode equals e positive" - hashCodeEqualsEPositive
      "hashCode equals e positive #2" - hashCodeEqualsEPositive2
      "hashCode not equals e positive" - hashCodeNotEqualsEPositive
      "hashCode not equals e positive #2" - hashCodeNotEqualsEPositive2
      "convert toUnsafe" - toUnsafe
      "equals" - testEquals
      "copy" - testCopy
      "failing copy with NumberFormatException" - testCopyFail
    }

    def readLongJNumber =
      forAll { (l: Long) =>
        sjsonnew.shaded.scalajson.ast.JNumber(l).value == l.toString
      }.checkUTest()

    def readBigDecimalJNumber =
      forAll { (b: BigDecimal) =>
        sjsonnew.shaded.scalajson.ast.JNumber(b).value == b.toString()
      }.checkUTest()

    def readBigIntJNumber =
      forAll { (b: BigInt) =>
        sjsonnew.shaded.scalajson.ast.JNumber(b).value == b.toString
      }.checkUTest()

    def readIntJNumber =
      forAll { (i: Int) =>
        sjsonnew.shaded.scalajson.ast.JNumber(i).value == i.toString
      }.checkUTest()

    def readDoubleJNumber =
      forAll { (d: Double) =>
        sjsonnew.shaded.scalajson.ast.JNumber(d) match {
          case sjsonnew.shaded.scalajson.ast.JNull => JNull == JNull
          case sjsonnew.shaded.scalajson.ast.JNumber(value) => value == d.toString
        }
      }.checkUTest()

    def readDoubleNANJNumber = {
      sjsonnew.shaded.scalajson.ast.JNumber(Double.NaN) match {
        case sjsonnew.shaded.scalajson.ast.JNull => true
        case _ => false
      }
    }

    def readDoublePositiveInfinityJNumber = {
      sjsonnew.shaded.scalajson.ast.JNumber(Double.PositiveInfinity) match {
        case sjsonnew.shaded.scalajson.ast.JNull => true
        case _ => false
      }
    }

    def readDoubleNegativeInfinityJNumber = {
      sjsonnew.shaded.scalajson.ast.JNumber(Double.NegativeInfinity) match {
        case sjsonnew.shaded.scalajson.ast.JNull => true
        case _ => false
      }
    }

    def readFloatJNumber =
      forAll { (f: Float) =>
        sjsonnew.shaded.scalajson.ast.JNumber(f) match {
          case sjsonnew.shaded.scalajson.ast.JNull => JNull == JNull
          case sjsonnew.shaded.scalajson.ast.JNumber(value) => value == f.toString
        }
      }.checkUTest()

    def readFloatNANJNumber = {
      sjsonnew.shaded.scalajson.ast.JNumber(Float.NaN) match {
        case sjsonnew.shaded.scalajson.ast.JNull => true
        case _ => false
      }
    }

    def readFloatPositiveInfinityJNumber = {
      sjsonnew.shaded.scalajson.ast.JNumber(Float.PositiveInfinity) match {
        case sjsonnew.shaded.scalajson.ast.JNull => true
        case _ => false
      }
    }

    def readFloatNegativeInfinityJNumber = {
      sjsonnew.shaded.scalajson.ast.JNumber(Float.NegativeInfinity) match {
        case sjsonnew.shaded.scalajson.ast.JNull => true
        case _ => false
      }
    }

    def readShortJNumber =
      forAll { (s: Short) =>
        sjsonnew.shaded.scalajson.ast.JNumber(s).value == s.toString
      }.checkUTest()

    def hashCodeEqualsDecimal = {
      sjsonnew.shaded.scalajson.ast.JNumber.fromString("34").get.## == sjsonnew.shaded.scalajson.ast.JNumber
        .fromString("34.0")
        .get
        .##
    }

    def hashCodeEqualsDecimal2 = {
      sjsonnew.shaded.scalajson.ast.JNumber.fromString("34").get.## == sjsonnew.shaded.scalajson.ast.JNumber
        .fromString("34.00")
        .get
        .##
    }

    def hashCodeNotEqualsDecimal = {
      sjsonnew.shaded.scalajson.ast.JNumber.fromString("34").get.## == sjsonnew.shaded.scalajson.ast.JNumber
        .fromString("34.01")
        .get
        .##
    }

    def hashCodeNotEqualsDecimal2 = {
      sjsonnew.shaded.scalajson.ast.JNumber.fromString("34").get.## == sjsonnew.shaded.scalajson.ast.JNumber
        .fromString("34.001")
        .get
        .##
    }

    def hashCodeEqualsE = {
      sjsonnew.shaded.scalajson.ast.JNumber.fromString("34e34").get.## != sjsonnew.shaded.scalajson.ast.JNumber
        .fromString("34e034")
        .get
        .##
    }

    def hashCodeEqualsE2 = {
      sjsonnew.shaded.scalajson.ast.JNumber.fromString("34e34").get.## != sjsonnew.shaded.scalajson.ast.JNumber
        .fromString("34e0034")
        .get
        .##
    }

    def hashCodeEqualsENegative = {
      JNumber("34e-0").## == JNumber("34").##
    }

    def hashCodeEqualsENegative2 = {
      JNumber("34e-00").## == JNumber("34").##
    }

    def hashCodeNotEqualsENegative = {
      JNumber("34e-01").## != JNumber("34").##
    }

    def hashCodeNotEqualsENegative2 = {
      JNumber("34e-001").## != JNumber("34").##
    }

    def hashCodeEqualsEPositive = {
      JNumber("34e+0").## == JNumber("34").##
    }

    def hashCodeEqualsEPositive2 = {
      JNumber("34e+00").## == JNumber("34").##
    }

    def hashCodeNotEqualsEPositive = {
      JNumber("34e+01").## != JNumber("34").##
    }

    def hashCodeNotEqualsEPositive2 = {
      JNumber("34e+001").## != JNumber("34").##
    }

    def toJsAny =
      forAll { (d: Double) =>
        sjsonnew.shaded.scalajson.ast.JNumber(d).toJsAny == (d: Any)
      }.checkUTest()

    def toUnsafe =
      forAll { (b: BigDecimal) =>
        sjsonnew.shaded.scalajson.ast.JNumber(b).toUnsafe == sjsonnew.shaded.scalajson.ast.unsafe.JNumber(b)
      }.checkUTest()

    def testEquals =
      forAll { (b: BigDecimal) =>
        sjsonnew.shaded.scalajson.ast.JNumber(b) == sjsonnew.shaded.scalajson.ast.JNumber(b)
      }.checkUTest()

    def testCopy =
      forAll { (b1: BigDecimal, b2: BigDecimal) =>
        val asString = b2.toString()
        sjsonnew.shaded.scalajson.ast.JNumber(b1).copy(value = asString) == sjsonnew.shaded.scalajson.ast
          .JNumber(b2)
      }.checkUTest()

    def testCopyFail =
      forAll { (b: BigDecimal) =>
        try {
          sjsonnew.shaded.scalajson.ast.JNumber(b).copy(value = "not a number")
          false
        } catch {
          case exception: NumberFormatException
              if exception.getMessage == "not a number" =>
            true
          case _ => false
        }
      }.checkUTest()
  }
}
