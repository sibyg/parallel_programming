package week3

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

/**
  * Never modify parallel collection on
  * which data parallel operation is in progress
  */
object ParallelGraphContraction {
  def main(arg: Array[String]): Unit = {
    def updatewhileReadingParallel() {
      val graph = mutable.Map[Int, Int]()
      graph ++= (0 until 100000).map(i => (i, i + 1))
      graph(graph.size - 1) = 0
      for ((k, v) <- graph.par) graph(k) = graph(v)
      val violation = graph.find({ case (k, v) => v != (k + 2) % graph.size })
      println(s"Concurrent Violation:$violation")
    }

    def updateUsingTrieMap() {
      val graph = TrieMap[Int, Int]()
      graph ++= (0 until 100000).map(i => (i, i + 1))
      graph(graph.size - 1) = 0
      val previous = graph.snapshot()
      for ((k, v) <- graph.par) graph(k) = previous(v)
      val violation = graph.find({ case (k, v) => v != (k + 2) % graph.size })
      println(s"TrieMap Violation:$violation")
    }

    updatewhileReadingParallel()
    updateUsingTrieMap()
  }
}
