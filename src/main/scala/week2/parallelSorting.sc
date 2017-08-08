import common._

val maxDepth = 10 // number of cores
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

if (maxDepth % 2 == 0) copy(ys, xs, 0, xs.length, 0)