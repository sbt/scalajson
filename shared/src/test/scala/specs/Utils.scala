package specs

object Utils {

  /**
    * Since [[sjsonnew.shaded.scalajson.ast.unsafe.JValue]] has mutable data-structures, it doesn't do structural equality
    * by default. Also takes care of the [[scalajson.unsafe.JObject]] special case with duplicate keys/ordering
    * @param left
    * @param right
    * @return
    */
  def unsafeJValueEquals(left: sjsonnew.shaded.scalajson.ast.unsafe.JValue,
                         right: sjsonnew.shaded.scalajson.ast.unsafe.JValue): Boolean = {
    (left, right) match {
      case (l: sjsonnew.shaded.scalajson.ast.unsafe.JString,
            r: sjsonnew.shaded.scalajson.ast.unsafe.JString) =>
        l == r
      case (sjsonnew.shaded.scalajson.ast.unsafe.JNull, sjsonnew.shaded.scalajson.ast.unsafe.JNull) =>
        true
      case (l: sjsonnew.shaded.scalajson.ast.unsafe.JNumber,
            r: sjsonnew.shaded.scalajson.ast.unsafe.JNumber) =>
        l == r
      case (l: sjsonnew.shaded.scalajson.ast.unsafe.JArray, r: sjsonnew.shaded.scalajson.ast.unsafe.JArray) =>
        val rValue = r.value
        l.value.zipWithIndex.forall {
          case (value, index) =>
            unsafeJValueEquals(value, rValue(index))
        }
      case (l: sjsonnew.shaded.scalajson.ast.unsafe.JBoolean,
            r: sjsonnew.shaded.scalajson.ast.unsafe.JBoolean) =>
        l == r
      case (l: sjsonnew.shaded.scalajson.ast.unsafe.JObject,
            r: sjsonnew.shaded.scalajson.ast.unsafe.JObject) =>
        val rAsMap = r.value.map { field =>
          (field.field, field.value)
        }.toMap
        val lAsMap = l.value.map { field =>
          (field.field, field.value)
        }.toMap
        rAsMap.forall {
          case (k, value) =>
            lAsMap.get(k) match {
              case Some(lValue) => unsafeJValueEquals(lValue, value)
              case _ => false
            }
        }

      case _ => false
    }
  }

}
