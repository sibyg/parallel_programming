package week3

import java.util.concurrent.ConcurrentSkipListSet

import scala.collection.{GenSet, mutable}

object IntersectionAnalysis {

  def main(arg: Array[String]) {

    def intersectionWrong(a: GenSet[Int], b: GenSet[Int]): mutable.Set[Int] = {
      val result = mutable.Set[Int]()
      for (x <- a) {
        if (b.contains(x)) result += x
      }
      result
    }

    val seqres = intersectionWrong((0 until 1000).toSet, (0 until 1000 by 4).toSet)
    val parres = intersectionWrong((0 until 1000).par.toSet, (0 until 1000 by 4).par.toSet)

    println(s"SeqRes:${seqres.size}, ParRes:${parres.size}")

    // Avoid mutation to the same memory location without proper synchronisation

    def intersectionRight(a: GenSet[Int], b: GenSet[Int]) = {
      val result = new ConcurrentSkipListSet[Int]
      for (x <- a) {
        if (b.contains(x)) result.add(x)
      }
      result
    }

    val seqres2 = intersectionRight((0 until 1000).toSet, (0 until 1000 by 4).toSet)
    val parres2 = intersectionRight((0 until 1000).par.toSet, (0 until 1000 by 4).par.toSet)

    println(s"SeqRes:${seqres2.size}, ParRes:${parres2.size}")


    def interectionWithRightCombinators(a: GenSet[Int], b: GenSet[Int]) = {
      if (a.size > b.size) {
        a.filter(b(_))
      } else {
        b.filter(a(_))
      }
    }

    val seqres3= interectionWithRightCombinators((0 until 1000).toSet, (0 until 1000 by 4).toSet)
    val parres3 = interectionWithRightCombinators((0 until 1000).par.toSet, (0 until 1000 by 4).par.toSet)

    println(s"SeqRes:${seqres3.size}, ParRes:${parres3.size}")

  }
}
