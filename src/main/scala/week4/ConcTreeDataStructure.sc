

//sealed trait Tree[+T]
//
//case class Node[T](left: Tree[T], right: Tree[T]) extends Tree[T]
//
//case class Leaf[T](elem: T) extends Tree[T]
//
//case object Empty extends Tree[Nothing]
//
//def filter[T](t: Tree[T], p: T => Boolean): Tree[T] = t match {
//  case Empty => Empty
//  case Leaf(v) => if (p(v)) t else Empty
//  case Node(l, r) =>
//    val (fL, fR) = parallel(filter(l, p), filter(r, p))
//    Node(fL, fR)
//
//}


sealed trait Conc[+T] {
  def level: Int

  def size: Int

  def left: Conc[T]

  def right: Conc[T]

  def <>(that: Conc[T]): Conc[T] = {
    if (this == Empty) that
    else if (that == Empty) this
    else concat(this, that)
  }

  def concat(xs: Conc[T], ys: Conc[T]): Conc[T] = {
    val diff = ys.level - xs.level
    if (diff >= -1 && diff <= 1) { // when two trees have height difference of 1 or less
      new <>(xs, ys)
    }
    else if (diff < -1) { // left tree is higher than the right one

      if (xs.left.level >= xs.right.level) { // case 1 - the left tree is left leaning
        val nr = concat(xs.right, ys)
        new <>(xs.left, nr)
      } else { // case 2 - the left tree is right leaning
        val nrr = concat(xs.right.right, ys)
        if (nrr.level == xs.level - 3) {
          val nl = xs.left
          val nr = new <>(xs.right.left, nrr)
          new <>(nl, nr)
        } else {
          val nl = new <>(xs.left, xs.right)
          val nr = nrr
          new <>(nl, nrr)
        }
      }
    } else { // right tree is higher than the left one
      ???
    }
  }

  var xs: Conc[T] = Empty

  def +=(elem: T) {
    xs = xs <> Single(elem)
  }
}

/**
  * same as class <> but with no invariants
  *
  * @param left
  * @param right
  * @tparam T
  */
case class Append[T](left: Conc[T], right: Conc[T]) extends Conc[T] {
  override def level: Int = 1 + math.max(left.level, right.level)

  override def size: Int = left.size + right.size

  def appendLeaf[T](xs: Conc[T], y: T): Conc[T] = Append(xs, Single(y))
}

/**
  * Constraints
  * a. A <> node can never have Empty as subtree
  * b. level difference between left and right subtree is always less than 1
  *
  * @tparam T
  */
case class <>[T](left: Conc[T], right: Conc[T]) extends Conc[T] {
  override def level: Int = 1 + math.max(left.level, right.level)

  override def size: Int = left.size + right.size


}

case class Single[T](elem: T) extends Conc[T] {
  override def level: Int = 0

  override def size: Int = 1

  override def left: Conc[T] = throw new Error("left on Leaf")

  override def right: Conc[T] = throw new Error("right on Leaf")
}

case object Empty extends Conc[Nothing] {
  override def level: Int = 0

  override def size: Int = 0

  override def left: Conc[Nothing] = throw new Error("left on Empty")

  override def right: Conc[Nothing] = throw new Error("right on Empty")
}