package specs

import org.scalactic.{Equality, TolerantNumerics}
import scala.scalajs.js

/**
  * Custom equality instances specifically for Scala.js
  */
object EqualityImplicits {
  implicit val doubleEq: Equality[Double] =
    TolerantNumerics.tolerantDoubleEquality(1e-4f)

  private def baseEquals(a: scalajson.ast.JValue, b: Any): Boolean = {
    val bAny = b.asInstanceOf[js.Any]

    a match {
      case scalajson.ast.JBoolean(bool) =>
        if (js.typeOf(bAny) == "boolean")
          bool == bAny.asInstanceOf[Boolean]
        else
          false
      case scalajson.ast.JNumber(value) =>
        if (js.typeOf(bAny) == "number") {
          // All numbers in Javascript are actually double, so do a tolerant
          // equal with an epsilon
          doubleEq.areEqual(value.toDouble, bAny.asInstanceOf[Double])
        } else
          false
      case scalajson.ast.JNull =>
        bAny == null
      case scalajson.ast.JString(value) =>
        if (js.typeOf(bAny) == "string")
          value == bAny.asInstanceOf[String]
        else
          false
      case scalajson.ast.JArray(values) =>
        if (js.typeOf(bAny) == "object" && js.Array.isArray(bAny)) {
          val asArray = bAny.asInstanceOf[js.Array[js.Any]]
          values.length == asArray.length && values.zipWithIndex.forall {
            case (value, i) =>
              baseEquals(value, asArray(i))
          }
        } else
          false
      case scalajson.ast.JObject(map) =>
        if (js.typeOf(bAny) == "object") {
          val asDictionary = bAny.asInstanceOf[js.Dictionary[js.Any]]
          map.size == asDictionary.size && map.forall {
            case (k, v) =>
              baseEquals(v, asDictionary(k))
          }
        } else
          false
    }
  }

  private def baseEqualsUnsafe(
      a: scalajson.ast.unsafe.JValue,
      b: Any
  ): Boolean = {
    val bAny = b.asInstanceOf[js.Any]

    a match {
      case scalajson.ast.unsafe.JBoolean(bool) =>
        if (js.typeOf(bAny) == "boolean")
          bool == bAny.asInstanceOf[Boolean]
        else
          false
      case scalajson.ast.unsafe.JNumber(value) =>
        if (js.typeOf(bAny) == "number") {
          // All numbers in Javascript are actually double, so do a tolerant
          // equal with an epsilon
          doubleEq.areEqual(value.toDouble, bAny.asInstanceOf[Double])
        } else
          false
      case scalajson.ast.unsafe.JNull =>
        bAny == null
      case scalajson.ast.unsafe.JString(value) =>
        if (js.typeOf(bAny) == "string")
          value == bAny.asInstanceOf[String]
        else
          false
      case scalajson.ast.unsafe.JArray(values) =>
        if (js.typeOf(bAny) == "object" && js.Array.isArray(bAny)) {
          val asArray = bAny.asInstanceOf[js.Array[js.Any]]
          values.length == asArray.length && values.zipWithIndex.forall {
            case (value, i) =>
              baseEqualsUnsafe(value, asArray(i))
          }
        } else
          false
      case scalajson.ast.unsafe.JObject(fields) =>
        if (js.typeOf(bAny) == "object") {
          val asDictionary = bAny.asInstanceOf[js.Dictionary[js.Any]]

          val distinctByField = fields.groupBy(_.field)

          distinctByField.size == asDictionary.size && distinctByField
            .forall {
              case (k, jFields) =>
                jFields.length match {
                  case 0 =>
                    // Should never happen
                    true
                  case 1 =>
                    baseEqualsUnsafe(jFields.head.value, asDictionary(k))
                  case _ =>
                    // Case where we have multiple values per key
                    jFields.exists { jField =>
                      baseEqualsUnsafe(jField.value, asDictionary(k))
                    }
                }
            }
        } else
          false
    }
  }

  /**
    * Designed to compare a [[scalajson.ast.JValue]] to its converted [[js.Any]] to
    * see if they are equal
    */
  implicit val jsValueEq: Equality[scalajson.ast.JValue] =
    new Equality[scalajson.ast.JValue] {
      override def areEqual(a: scalajson.ast.JValue, b: Any): Boolean =
        baseEquals(a, b)
    }

  /**
    * Designed to compare a [[scalajson.ast.unsafe.JValue]] to its converted [[js.Any]] to
    * see if they are equal
    */
  implicit val jsUnsafeValueEq: Equality[scalajson.ast.unsafe.JValue] =
    new Equality[scalajson.ast.unsafe.JValue] {
      override def areEqual(a: scalajson.ast.unsafe.JValue, b: Any): Boolean =
        baseEqualsUnsafe(a, b)
    }
}
