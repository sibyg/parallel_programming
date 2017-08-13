import common._

/**
  * conventional quick sort function
  *
  * @param input unsorted array to be sorted
  * @return
  */
def quickSort(input: Array[Int]): Array[Int] = {
  if (input.length < 2) {
    input
  } else {
    val pivot = input(input.length / 2)
    quickSort(input filter (_ < pivot)) ++ (input filter (_ == pivot)) ++ quickSort(input filter (_ > pivot))
  }
}

def printArray[A](arr: Array[A]) = {
  arr.deep.mkString(",")
}

val unsortedArray = Array(6, 3, 7, 4)
val sortedArray = quickSort(unsortedArray)
printArray(sortedArray)

//val xs = Array(6, 3, 7, 4)
val xs = Array(8, 4, 17, 10, 3, 2)

def indexAndLengthBasedQuickSort(inp: Array[Int], from: Int, length: Int): Unit = {
  val ys = new Array[Int](length)
  var ysIdx = 0
  while (ysIdx < length) {
    ys(ysIdx) = inp(from + ysIdx)
    ysIdx = ysIdx + 1
  }

  val result = quickSort(ys)
  var resultIdx = 0
  while (resultIdx < length) {
    inp(from + resultIdx) = result(resultIdx)
    resultIdx = resultIdx + 1
  }
}

//indexAndLengthBasedQuickSort(xs, 0, 2)
//printArray(xs)

def parMergeSort(xs: Array[Int], maxDepth: Int): Unit = {

  val ys = new Array[Int](xs.length)

  def sort(from: Int, until: Int, depth: Int): Unit = {
    // base-case
    if (depth == maxDepth) {
      indexAndLengthBasedQuickSort(xs, from, until - from)
    } else {
      val mid = (from + until) / 2
      parallel(sort(mid, until, depth + 1), sort(from, mid, depth + 1))

      val flip = (maxDepth - depth) % 2 == 0
      val src = if (flip) ys else xs
      val dst = if (flip) xs else ys
      merge(src, dst, from, mid, until)
    }
  }

  //TODO to be refined
  def merge(src: Array[Int], target: Array[Int], from: Int, mid: Int, until: Int): Unit = {
    var left = from
    var right = mid
    var i = from
    while (left < mid && right < until) {
      while (left < mid && src(left) <= src(right)) {
        target(i) = src(left)
        i += 1
        left += 1
      }
      while (right < until && src(right) <= src(left)) {
        target(i) = src(right)
        i += 1
        right += 1
      }
    }
    while (left < mid) {
      target(i) = src(left)
      i += 1
      left += 1
    }
    while (right < until) {
      target(i) = src(right)
      i += 1
      right += 1
    }
  }

  def copy(src: Array[Int], target: Array[Int],
           from: Int, until: Int, depth: Int): Unit = {

    if (depth == maxDepth) {
      Array.copy(src, from, target, from, until - from)
    } else {
      val mid = (from + until) / 2
      val right = parallel(
        copy(src, target, from, mid, depth + 1),
        copy(src, target, mid + 1, until, depth + 1),
      )
    }
  }

  sort(0, xs.length, 0)
  if (maxDepth % 2 == 0) copy(ys, xs, 0, xs.length, 0)
}






