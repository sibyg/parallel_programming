import common._

// REFERENCE - ONLY
//sealed trait List[+T] {
//  def head: T
//
//  def tail: List[T]
//}
//
//case class ::[T](head: T, tail: List[T]) extends List[T]

//case object Nil extends List[Nothing] {
//  def head = sys.error("empty list")
//
//  def tail = sys.error("empty list")
//}

def filter[T](lst: List[T])(p: T => Boolean): List[T] = lst match {
  case Nil => Nil
  case head :: tail => if (p(head)) head :: filter(tail)(p) else filter(tail)(p)
}

// REFERENCE - ONLY
//sealed trait Tree[+T]
//case class Leaf[T](elem: T) extends Tree[T]
//case class Node[T](left: Tree[T], right: Tree[T]) extends Tree[T]
//case object Empty extends Tree[Nothing]


//def filter[T](tree: Tree[T])(p: T=> Boolean): Tree[T] = tree match {
//  case Empty => Empty
//  case Leaf(v) => if (p(v)) tree else Empty
//  case Node(l, r) =>
//    val tuple = parallel(filter(l)(p), filter(r)(p))
//    Node(tuple._1, tuple._2)
//}


sealed trait Conc[+T] {
  def level: Int

  def size: Int

  def left: Conc[T]

  def right: Conc[T]

  def <>[T](that: Conc[T]): Conc[T] = {
    if (this == Empty) that
    else if (that == Empty) this
    else concat(this, that)
  }

  def concat[T](xs: Conc[T], ys: Conc[T]): Conc[T] = {
    val diff = ys.level - xs.level
    if (diff >= -1 && diff <= 1) new <>(xs, ys) // invariants satisfied
    else if (diff < -1) { // left tree is higher than the right one
      if (xs.left.level >= xs.right.level) {
        val nr = concat(xs.right, ys)
        new <>(xs.left, nr)
      } else {
        val nrr = concat(xs.right.right, ys)
        if (nrr.level == xs.level - 3) {
          val nl = xs.left
          val nr = new <>(xs.right.left, nrr)
          new <>(nl, nr)
        } else {
          val nl = new <>(xs.left, xs.right.left)
          val nr = nrr
          new <>(nl, nr)
        }
      }
    } else {
      throw new Error("Never happening situation!!!")
    }
  }
}

case object Empty extends Conc[Nothing] {
  override def level = 0

  override def size = 0

  override def left = throw new Error("left in Empty")

  override def right = throw new Error("right in Empty")
}

case class Single[T](x: T) extends Conc[T] {
  override def level = 0

  override def size = 1

  override def left = throw new Error("left in Single")

  override def right = throw new Error("right in Single")
}

/**
  * Invariants
  * a. A <> node can never contain Empty as its subtree
  * b. The level difference between the left and the right subtree
  * of a <> node is always 1 or less
  *
  * @param left
  * @param right
  * @tparam T
  */
case class <>[T](left: Conc[T], right: Conc[T]) extends Conc[T] {
  override def level = 1 + math.max(left.level, right.level)

  override def size = left.size + right.size
}

