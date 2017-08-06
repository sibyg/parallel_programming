List(1, 3, 8).scanLeft(100)((s, x) => s + x)
List(1, 3, 8).scanRight(100)((s, x) => s + x)

def scanLeft[A](arr: Array[A], out: Array[A], a0: A, f: (A, A) => A) = {
  out(0) = a0
  for (i <- 1 to arr.length) {
    out(i) = f(out(i - 1), arr(i - 1))
  }
}


val arr = Array(1, 3, 8)
val out = Array(0, 0, 0, 0)
def f(x: Int, y: Int): Int = x + y
def fi(index: Int, value: Int): Int = value * value

scanLeft(arr, out, 100, f)
out

def reduceSeg1[A](inp: Array[A], left: Int, right: Int, a0: A,
                  f: (A, A) => A): A = {
  var result = f(a0, inp(left))
  for (i <- left + 1 to right) {
    result = f(result, inp(i))
  }
  result
}

reduceSeg1(arr, 1,2,100, f)

def mapSeg[A, B](inp: Array[A], left: Int,
                 right: Int, fi: (Int, A) => B, out: Array[B]): Unit = {
  for (i <- left to right) {
    out(i) = fi(i, inp(i))
  }
}

mapSeg(arr, 1, 2, fi, out)
out