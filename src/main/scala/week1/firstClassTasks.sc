import common._;

def sumSegment(arr: Array[Int], p: Double, start: Int, end: Int) = {
  var sum: Double = 0
  for (i <- start until end) {
    sum = sum + Math.pow(Math.abs(arr(i)), p)
    )
  }
  sum
}

val a: Array[Int]
val p: Int = 2

val mid1 = a.length / 4
val mid2 = a.length / 2
val mid3 = 3 / 4 * a.length

val t1 = task {sumSegment(a, p, 0, mid1)}
val t2 = task {sumSegment(a, p, mid1, mid2)}
val t3 = task {sumSegment(a, p, mid2, mid3)}
val t4 = task {sumSegment(a, p, mid3, a.length)}

Math.pow(t1+t2+t3+t4, 1/p)