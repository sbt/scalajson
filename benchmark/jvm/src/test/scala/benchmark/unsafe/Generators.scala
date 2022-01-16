package benchmark.unsafe

import org.scalameter._
import sjsonnew.shaded.scalajson.ast.unsafe

object Generators {

  def jBoolean: Gen[unsafe.JBoolean] =
    for {
      size <- Gen.range("seed")(300000, 1500000, 300000)
    } yield {
      if (size % 2 == 0)
        unsafe.JBoolean(true)
      else
        unsafe.JBoolean(false)
    }

  def jString: Gen[unsafe.JString] =
    for {
      size <- Gen.range("seed")(300000, 1500000, 300000)
    } yield {
      unsafe.JString(size.toString)
    }

  def jNumber: Gen[unsafe.JNumber] =
    for {
      size <- Gen.range("seed")(300000, 1500000, 300000)
    } yield {
      unsafe.JNumber(size)
    }

  def jArray: Gen[unsafe.JArray] =
    for {
      size <- Gen.range("seed")(0, 10, 1)
      randomJValue <- jValue
    } yield {

      val array: Array[unsafe.JValue] = Array.ofDim(size)
      (0 until size).foreach { index =>
        array(index) = randomJValue
      }
      unsafe.JArray(array)
    }

  def jObject: Gen[unsafe.JObject] =
    for {
      size <- Gen.range("seed")(0, 10, 1)
      string <- Gen.range("seed")(300000, 1500000, 300000).map {
        _.toString
      }
      randomJValue <- jValue
    } yield {
      val array: Array[unsafe.JField] = Array.ofDim(size)
      (0 until size).foreach { index =>
        array(index) = unsafe.JField(string, randomJValue)
      }
      unsafe.JObject(array)
    }

  def jValue: Gen[unsafe.JValue] =
    Gen.range("JValue type")(300000, 1500000, 300000).flatMap { seed =>
      seed % 5 match {
        case 0 => jBoolean.asInstanceOf[Gen[unsafe.JValue]]
        case 1 => jString.asInstanceOf[Gen[unsafe.JValue]]
        case 2 => jNumber.asInstanceOf[Gen[unsafe.JValue]]
        case 3 => jArray.asInstanceOf[Gen[unsafe.JValue]]
        case 4 => JArray.asInstanceOf[Gen[unsafe.JValue]]
      }
    }
}
