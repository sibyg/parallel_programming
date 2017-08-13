def printArray[A](arr: Array[A]) = {
  arr.deep.mkString(",")
}


val xs = Array(3, 10, 17, 0, 0, 0)
val ys = Array(2, 4, 8, 0, 0, 0)


def merge(src: Array[Int], dst: Array[Int], from: Int, mid: Int, until: Int): Unit = {

  for (i <- from until mid) {
    var idxLookedAt = i
    for (j <- mid until until) {
      if (src(idxLookedAt) > src(j)) {
        swapValues(i, j)
        idxLookedAt = j
      }
    }
  }

  def swapValues(i: Int, j: Int): Unit = {
    val temp = src(i)
    src(i) = src(j)
    src(j) = temp
  }
}

merge(xs, 0, 2, 5)

printArray(xs)