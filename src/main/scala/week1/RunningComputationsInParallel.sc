import common._

def pNormMine(a: Array[Int], p: Double, n: Int): Unit = {
  a.foldLeft(0, Math.pow(_, p))
}

def pNorm(a: Array[Int], p: Double): Int = power(sumSegment(a, p, 0, a.length), 1 / p)

def sumSegment(a: Array[Int], p: Double, s: Int, t: Int): Int = {
  var sum: Int = 0
  var i = s
  while (i < t) {
    sum = sum + power(a(i), p)
  }
  sum
}

def power(x: Int, p: Double): Int = math.exp(p * math.log(math.abs(x))).toInt

def pNormPartTwo(a: Array[Int], p: Double): Int = {
  val m1 = a.length / 4
  val m2 = a.length / 2
  val m3 = a.length * (3 / 4)
  val (sum1, sum2, sum3, sum4) = parallel(sumSegment(a, p, 0, m1),
    sumSegment(a, p, m1 + 1, m2),
    sumSegment(a, p, m2 + 1, m3),
    sumSegment(a, p, m3 + 1, a.length))
  power(sum1 + sum2 + sum3 + sum4, 1 / p)
}

def pNormRec(a: Array[Int], p: Double, threshold: Int): Int = {
  val mid = a.length / 2
  if (mid < threshold) {
    power(sumSegment(a, p, 0, mid) + sumSegment(a, p, mid, a.length), 1 / p)
  } else {
    pNormRec(a)
  }
}