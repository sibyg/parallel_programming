import scala.collection.mutable.ArrayBuffer

val xs = ArrayBuffer(3, 10, 17, 0, 0, 0)
val ys = ArrayBuffer(2, 4, 8, 9, 0, 0, 0)
val parallelism = 3
val numElems = 13
val steps = math.max(1, numElems / parallelism)
val starts = (0 until numElems by steps) :+ numElems
steps

def printArray[A](arr: Array[A]) = {
  arr.deep.mkString(",")
}
//val chunks  = starts.zip(starts.tail)