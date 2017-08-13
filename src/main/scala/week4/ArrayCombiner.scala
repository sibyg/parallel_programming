package week4

import common._
import org.scalameter

import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.Combiner
import scala.reflect.ClassTag
import org.scalameter._

class ArrayCombiner[T <: AnyRef : ClassTag](val parallelism: Int)
  extends Combiner[T, Array[T]] {
  private var numElems = 0
  private val buffers = new ArrayBuffer[ArrayBuffer[T]]
  buffers += new ArrayBuffer[T]

  def +=(x: T) = {
    buffers.last += x
    numElems += 1
    this
  }

  override def combine[N <: T, That >: Array[T]](that: Combiner[N, That]) = {
    (that: @unchecked) match {
      case that: ArrayCombiner[T] =>
        buffers ++= that.buffers
        numElems + that.numElems
        this
    }
  }

  def size = numElems

  def clear() = buffers.clear()

  def result: Array[T] = {
    val array = new Array[T](numElems)
    val step = math.max(1, numElems / parallelism)
    val starts = (0 until numElems by step) :+ numElems
    val chunks = starts.zip(starts.tail)
    val tasks = for ((from, end) <- chunks) yield task {
      copyTo(array, from, end)
    }
    tasks.foreach(_.join())
    array
  }

  def copyTo(array: Array[T], from: Int, end: Int): Unit = {
    var i = from
    var j = 0

    // identify right inner buffer
    while (i >= buffers(j).length) {
      i -= buffers(j).length
      j += 1
    }

    // traverse through the buffer
    var k = from
    while (k < end) {
      array(k) = buffers(j)(i)
      i += 1
      if (i >= buffers(j).length) {
        i = 0
        j += 1
      }
      k += 1
    }
  }

}

object ArrayCombiner {
  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 60,
    Key.verbose -> true
  ) withWarmer new scalameter.Warmer.Default

  def main(args: Array[String]) {
    val size = 1000000

    def run(p: Int): Unit = {
      val taskSupport = new collection.parallel.ForkJoinTaskSupport {
        new scala.concurrent.forkjoin.ForkJoinPool(p)
        val strings = (0 until size).map(_.toString)
        val time = standardConfig measure {
          val parallelized = strings.par

          def newCombiner = new ArrayCombiner(p): Combiner[String, Array[String]]

          parallelized.aggregate(newCombiner)(_ += _, _ combine _).result
        }
        println(s"p=$p, time=$time ms")
      }
      run(1)
      run(2)
      run(4)
      run(8)
    }
  }
}

