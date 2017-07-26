def pNorm(a: Array[Int], p: Double, n: Int): Unit = {
  a.foldLeft(0, Math.pow(_, p))
}