import common.parallel

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
def sum = (x: Int, y: Int) => x + y
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

/**
  * scanLeft using map and reduce
  *
  * @param inp
  * @param a0
  * @param f
  * @param out
  * @tparam A
  */
def scanLeftVer2[A](inp: Array[A], a0: A, f: (A, A) => A, out: Array[A]) = {
  val fi = {
    (i: Int, v: A) => reduceSeg1(inp, 0, i, a0, f)
  }
  mapSeg(inp, 0, inp.length, fi, out)
  val last = inp.length - 1
  out(last + 1) = f(out(last), inp(last))
}


sealed abstract class Tree[A]

case class Leaf[A](a: A) extends Tree[A]

case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]

sealed abstract class TreeRes[A] {
  val res: A
}

case class LeafRes[A](override val res: A) extends TreeRes[A]

case class NodeRes[A](l: TreeRes[A], override val res: A, r: TreeRes[A]) extends TreeRes[A]

def reduceRes[A](t: Tree[A], f: (A, A) => A): TreeRes[A] = t match {
  case Leaf(v) => LeafRes(v)
  case Node(l, r) =>
    val lRes = reduceRes(l, f)
    val rRes = reduceRes(r, f)
    NodeRes(lRes, f(lRes.res, rRes.res), rRes)
}

val t1 = Node(Node(Leaf(1), Leaf(3)), Node(Leaf(8), Leaf(50)))
val plus = (x: Int, y: Int) => x + y
val res = reduceRes(t1, plus)

/**
  * parallel implementation of reduce
  *
  * @param t
  * @param f
  * @tparam A
  * @return
  */
def upsweep[A](t: Tree[A], f: (A, A) => A): TreeRes[A] = t match {
  case Leaf(v) => LeafRes(v)
  case Node(l, r) => {
    val (tL, tR) = parallel(upsweep(l, f), upsweep(r, f))
    NodeRes(tL, f(tL.res, tR.res), tR)
  }
}


def downsweep[A](t: TreeRes[A], a0: A, f: (A, A) => A): Tree[A] = t match {
  case LeafRes(v) => Leaf(f(a0, v))
  case NodeRes(l, _, r) => {
    val (tL, tR) = parallel(downsweep(l, a0, f), downsweep(r, f(a0, l.res), f))
    Node(tL, tR)
  }
}

val value = downsweep(res, 100, plus)

def scanLeft[A](t: Tree[A], a0: A, f: (A, A) => A): Tree[A] = {
  val tRes = upsweep(t, f)
  val scan1 = downsweep(tRes, a0, f)
  prepend(a0, scan1)
}

def prepend[A](a: A, value: Tree[A]): Tree[A] = value match {
  case Leaf(v) => Node(value, Leaf(a))
  case Node(l, r) => Node(prepend(a, l), r)
}

// ARRAY SCAN-LEFT
sealed abstract class TreeResA[A] {
  val res: A
}

case class LeafA[A](from: Int, to: Int, override val res: A)
  extends TreeResA[A]

case class NodeA[A](l: TreeResA[A], override val res: A, r: TreeResA[A])
  extends TreeResA[A]

val threshold = 3

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