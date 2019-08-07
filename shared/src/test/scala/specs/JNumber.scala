package specs

import scalajson.ast._

class JNumber extends Spec {

  "The JNumber value" should {
    "read a Long" in {
      forAll { l: Long =>
        JNumber(l).value should be(l.toString)
      }
    }

    "read a BigDecimal" in {
      forAll { b: BigDecimal =>
        JNumber(b).value should be(b.toString)
      }
    }

    "read a BigInt" in {
      forAll { b: BigInt =>
        JNumber(b).value should be(b.toString)
      }
    }

    "read an Int" in {
      forAll { i: Int =>
        JNumber(i).value should be(i.toString)
      }
    }

    "read a Double" in {
      forAll { d: Double =>
        JNumber(d) match {
          case JNull          => JNull should be(JNull)
          case JNumber(value) => value should be(d.toString)
        }
      }
    }

    "read a Double NaN" in {
      JNumber(Double.NaN) match {
        case JNull => true
        case _     => false
      }
    }

    "read a Double Positive Infinity" in {
      JNumber(Double.PositiveInfinity) match {
        case JNull => true
        case _     => false
      }
    }

    "read a Double Negative Infinity" in {
      JNumber(Double.NegativeInfinity) match {
        case JNull => true
        case _     => false
      }
    }

    "read a Float" in {
      forAll { f: Float =>
        {
          JNumber(f) match {
            case JNull          => JNull should be(JNull)
            case JNumber(value) => value should be(f.toString)
          }
        }
      }
    }

    "read a Float NaN" in {
      JNumber(Float.NaN) match {
        case JNull => true
        case _     => false
      }
    }

    "read a Float Positive Infinity" in {
      JNumber(Float.PositiveInfinity) match {
        case JNull => true
        case _     => false
      }
    }

    "read a Float Negative Infinity" in {
      JNumber(Float.NegativeInfinity) match {
        case JNull => true
        case _     => false
      }
    }

    "read a Short" in {
      forAll { s: Short =>
        JNumber(s).value should be(s.toString)
      }
    }

    "hashCode equals decimal" in {
      JNumber.fromString("34").get.## should be(
        JNumber.fromString("34.0").get.##
      )
    }

    "hashCode equals decimal #2" in {
      JNumber.fromString("34").get.## should be(
        JNumber.fromString("34.00").get.##
      )
    }

    "hashCode not equals decimal" in {
      JNumber.fromString("34").get.## should not be (JNumber
        .fromString("34.01")
        .get
        .##)
    }

    "hashCode not equals decimal #2" in {
      JNumber.fromString("34").get.## should not be (JNumber
        .fromString("34.001")
        .get
        .##)
    }

    "hashCode equals e" in {
      JNumber.fromString("34e34").get.## should be(
        JNumber
          .fromString("34e034")
          .get
          .##
      )
    }

    "hashCode equals e #2" in {
      JNumber.fromString("34e34").get.## should be(
        JNumber.fromString("34e0034").get.##
      )
    }

    "hashCode equals e negative" in {
      JNumber.fromString("34e-0").get.## should be(
        JNumber.fromString("34").get.##
      )
    }

    "hashCode equals e negative #2" in {
      JNumber.fromString("34e-00").get.## should be(
        JNumber.fromString("34").get.##
      )
    }

    "hashCode not equals e negative" in {
      JNumber.fromString("34e-01").get.## should not be (JNumber
        .fromString("34")
        .get
        .##)
    }

    "hashCode not equals e negative #2" in {
      JNumber.fromString("34e-001").get.## should not be (JNumber
        .fromString("34")
        .get
        .##)
    }

    "hashCode equals e positive" in {
      JNumber.fromString("34e+0").get.## should be(
        JNumber.fromString("34").get.##
      )
    }

    "hashCode equals e positive #2" in {
      JNumber.fromString("34e+00").get.## should be(
        JNumber.fromString("34").get.##
      )
    }

    "hashCode not equals e positive" in {
      JNumber.fromString("34e+01").get.## should not be (JNumber
        .fromString("34")
        .get
        .##)
    }

    "hashCode not equals e positive #2" in {
      JNumber.fromString("34e+001").get.## should not be (JNumber
        .fromString("34")
        .get
        .##)
    }

    "convert toUnsafe" in {
      forAll { b: BigDecimal =>
        scalajson.ast.JNumber(b).toUnsafe should be(
          scalajson.ast.unsafe.JNumber(b)
        )
      }
    }

    "equals" in {
      forAll { b: BigDecimal =>
        scalajson.ast.JNumber(b) should be(scalajson.ast.JNumber(b))

      }
    }

    "copy" in {
      forAll { (b1: BigDecimal, b2: BigDecimal) =>
        val asString = b2.toString()
        scalajson.ast.JNumber(b1).copy(value = asString) should be(
          scalajson.ast.JNumber(b2)
        )

      }
    }

    "failing copy with NumberFormatException" in {
      forAll { b: BigDecimal =>
        assertThrows[NumberFormatException] {
          scalajson.ast.JNumber(b).copy(value = "not a number")
        }
      }
    }
  }

}
