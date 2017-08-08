import common._

List(1, 3, 8).scanLeft(100)((s, x) => s + x)
List(1, 3, 8).scanRight(100)((s, x) => s + x)

val t1 = Node(Node(Leaf(1), Leaf(3)), Node(Leaf(8), Leaf(50)))


val arr = Array(1, 3, 8)
val out = Array(0, 0, 0, 0)
val threshold = 10
def fi(index: Int, value: Int): Int = value * value

scanLeft(arr, out, 100, sum)
out

def reduceSeg1[A](inp: Array[A], left: Int, right: Int, a0: A,
                  f: (A, A) => A): A = {
  var result = f(a0, inp(left))
  for (i <- left + 1 to right) {
    result = f(result, inp(i))
  }
  result
}

reduceSeg1(arr, 1, 2, 100, sum)

def mapSeg[A, B](inp: Array[A], left: Int,
                 right: Int, fi: (Int, A) => B, out: Array[B]): Unit = {
  for (i <- left to right) {
    out(i) = fi(i, inp(i))
  }
}

mapSeg(arr, 1, 2, fi, out)
out
val unit = scanLeft(arr, 100, sum, out)

// SCAN ARRAY - SEQ
def scanLeft[A](arr: Array[A], out: Array[A], a0: A, f: (A, A) => A) = {
  out(0) = a0
  for (i <- 1 to arr.length) {
    out(i) = f(out(i - 1), arr(i - 1))
  }
}

def sum = (x: Int, y: Int) => x + y

def reduceRes[A](t: Tree[A], f: (A, A) => A): TreeRes[A] = t match {
  case Leaf(v) => LeafRes(v)
  case Node(l, r) => {
    val (tL, tR) = (reduceRes(l, f), reduceRes(r, f))
    NodeRes(tL, f(tL.res, tR.res), tR)
  }
}

def plus = (x: Int, y: Int) => x + y

def upsweep[A](t: Tree[A], f: (A, A) => A): TreeRes[A] = t match {
  case Leaf(v) => LeafRes(v)
  case Node(l, r) => {
    val (tL, tR) = parallel(upsweep(l, f), upsweep(r, f))
    NodeRes(tL, f(tL.res, tR.res), tR)
  }
}

def downsweep[A](t: TreeRes[A], a0: A, f: (A, A) => A): Tree[A] = t match {
  case LeafRes(v) => Leaf(f(a0, v))
  case NodeRes(l, v, r) => {
    val (lDownSweep, rDownSweep) = parallel(downsweep(l, a0, f), downsweep(r, f(a0, l.res), f))
    Node(lDownSweep, rDownSweep)
  }
}

def scanLeft[A](t: Tree[A], a0: A, f: (A, A) => A): Tree[A] = {
  val tRes = upsweep(t, f)
  val scan1 = downsweep(tRes, a0, f)
  prepend(a0, scan1)
}

def prepend[A](x: A, t: Tree[A]): Tree[A] = t match {
  case Leaf(v) => Node(Leaf(x), Leaf(v))
  case Node(l, r) => Node(prepend(x, l), r)
}

reduceRes(t1, plus)

def upsweep[A](inp: Array[A], from: Int, to: Int, f: (A, A) => A): TreeResA[A] = {
  if (to - from < threshold) {
    LeafA(from, to, reduceSeg1(inp, from + 1, to, inp(from), f))
  } else {
    val mid = from + (to - from) / 2
    val (tL, tR) = parallel(upsweep(inp, from, mid, f), upsweep(inp, mid, to, f))
    NodeA(tL, f(tL.res, tR.res), tR)
  }
}

def downsweep[A](inp: Array[A], a0: A, f: (A, A) => A, t: TreeResA[A], out: Array[A]): Unit = t match {
  case LeafA(from, to, v) => scanLeftSeg(inp, from, to, a0, f, out)
  case NodeA(l, v, r) => {
    val (lDownSweep, rDownSweep) = parallel(downsweep(inp, a0, f, l, out), downsweep(inp, f(a0, l.res), f, r, out))
  }
}

def scanLeftSeg[A](inp: Array[A],
                   from: Int, to: Int, a0: A,
                   f: (A, A) => A, out: Array[A]) = {
  var tempRes = a0
  for (i <- from until to) {
    tempRes = f(tempRes, inp(i))
    out(i) = tempRes
  }
}

def scanLeft[A](inp: Array[A], a0: A, f: (A, A) => A, out: Array[A]) = {
  val tRes = upsweep(inp, 0, inp.length, f)
  val scan1 = downsweep(inp, a0, f, tRes, out)
  out(0) = a0
}

// TREE SCAN LEFT
sealed abstract class Tree[A]

// intermediate result
sealed abstract class TreeRes[A] {
  val res: A
}

// ARRAY SCAN-LEFT
sealed abstract class TreeResA[A] {
  val res: A
}

case class Leaf[A](a: A) extends Tree[A]

case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]

case class LeafRes[A](override val res: A) extends TreeRes[A]

case class NodeRes[A](l: TreeRes[A],
                      override val res: A, r: TreeRes[A]) extends TreeRes[A]

case class LeafA[A](from: Int, to: Int, override val res: A)
  extends TreeResA[A]

case class NodeA[A](l: TreeResA[A], override val res: A, r: TreeResA[A])
  extends TreeResA[A]

unit

