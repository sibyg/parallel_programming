/**
  * Sequential transformer operation
  *
  * @tparam T
  * @tparam Repr
  */
trait Builder[T, Repr] {
  def +=(elem: T): this.type

  def result: Repr
}

trait Combiner[T, Repr] extends Builder[T, Repr] {
  def combine(that: Combiner[T, Repr]): Combiner[T, Repr]
}


def combine(xs: Array[Int], ys: Array[Int]): Array[Int] = {
  val rs = new Array[Int](xs.length + ys.length)
  Array.copy(xs, 0, rs, 0, xs.length)
  Array.copy(ys, 0, rs, xs.length, ys.length)
  rs
}

combine(Array(1,2,3), Array(4,5,6))