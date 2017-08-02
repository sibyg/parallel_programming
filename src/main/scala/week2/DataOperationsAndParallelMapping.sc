import common._

List(1, 3, 8).map(x => x * x)

List(1, 3, 8).fold(100)((s, x) => s + x)

List(1, 3, 8).scan(100)((s, x) => s + x)

def mapASeqSeq[A, B](inp: Array[A], left: Int, right: Int, f: A => B, out: Array[B]) = {
  for (i <- left to right) {
    out(i) = f(inp(i))
  }
  out
}

val in = Array(2, 3, 4, 5, 6)
val out = Array(0, 0, 0, 0, 0)
val f = (x: Int) => x * x
mapASeqSeq(in, 1, 3, f, out)

val threshold = 10
def mapASeqPar[A, B](inp: Array[A], left: Int, right: Int, f: A => B, out: Array[B]): Unit = {
  if (right - left < threshold) {
    val mid = (right - left) / 2
    parallel(mapASeqSeq(inp, left, mid, f, out),
      mapASeqSeq(inp, left + 1, right, f, out))
  } else {
    mapASeqSeq(inp, left, right, f, out)
  }
}

sealed abstract class Tree[A] {
  val size: Int
}

case class Leaf[A](a: Array[A]) extends Tree[A] {
  override val size: Int = a.length
}

case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A] {
  override val size: Int = l.size + r.size
}

def mapTreePar[A: Manifest, B: Manifest](t: Tree[A], f: A => B): Tree[B] = {
  t match {
    case Leaf(a) => {
      val b = new Array[B](a.length)
      for (i <- 0 to a.length) {
        b(i) = f(a(i))
      }
      Leaf(b)
    }
    case Node(l, r) => {
      val (lb, rb) = parallel(mapTreePar(l, f), mapTreePar(r, f))
      Node(lb, rb)
    }
  }
}