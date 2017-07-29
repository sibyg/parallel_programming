import scala.util.Random
import common._

def mCount(iter: Int): Int = {
  val randomX = new Random
  val randomY = new Random
  var hits = 0
  for (i <- 0 until iter) {
    val x = randomX.nextDouble
    val y = randomY.nextDouble
    if (x * x + y * y < 1) hits = hits + 1
  }
  hits
}


def monteCarlPiSeq(iter: Int): Double
= 4.0 * mCount(iter) / iter

def monteCarlPiPar(iter: Int): Double = {
  val ((pi1, pi2), (pi3, pi4)) = parallel(
    parallel(mCount(iter / 4), mCount(iter / 4)),
    parallel(mCount(iter / 4), mCount(iter - 3 * (iter / 4))))
  4.0 * (pi1 + pi2 + pi3 + pi4) / iter
}