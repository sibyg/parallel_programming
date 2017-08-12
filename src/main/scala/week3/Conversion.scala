
package week3

import org.scalameter
import org.scalameter._

object Conversion {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 10,
    Key.exec.maxWarmupRuns -> 20,
    Key.exec.benchRuns -> 20,
    Key.verbose -> true
  ) withWarmer new scalameter.Warmer.Default

  val memConfig = config(
    Key.exec.minWarmupRuns -> 0,
    Key.exec.maxWarmupRuns -> 0,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer Warmer.Zero

  val vector = Vector.fill(1000000)("")
  val list = vector.toList

  def main(args: Array[String]) {
    val listtime = standardConfig measure {
      list.par
    }
    println(s"list conversion time: $listtime ms")

    val vectortime = standardConfig measure {
      vector.par
    }
    println(s"vector conversion time: $vectortime ms")
    val diff = listtime.value / vectortime.value
    println(s"difference: $diff")
  }
}
