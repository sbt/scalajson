package sjsonnew.shaded.scalajson.ast

import benchmark.Generators
import org.scalameter.Bench

/**
  * Created by matthewdedetrich on 17/10/16.
  */
object PrivateBenchmark extends Bench.ForkedTime {

  performance of "privateMethods" in {
    measure method "hashcode" in {
      using(Generators.jNumber) in { jNumber: JNumber =>
        jNumber.##
      }
    }

    measure method "equalsItself" in {
      using(Generators.jNumber) in { jNumber: JNumber =>
        jNumber.equals(jNumber)
      }
    }
  }
}
