import common._

trait iterator[T] {
  def next(): T

  def hasNext: Boolean

  def foldLeft[S](z: S)(f: (S, T) => S): S = {
    var s = z
    while (hasNext) {
      s = f(s, next())
    }
    s
  }
}

def iterator[A]: Iterator[A] // on every collection

val threshold = 2

trait Splitter[A] extends Iterator[A] {
  def split: Seq[Splitter[A]]

  def remaining: Int

  def fold(z: A)(f: (A, A)): A = {
    if (remaining < threshold) foldLeft(z)(f)
    else {
      val children = for (child <- split) yield task {
        child.fold(z)(f)
      }
      children.map(_.join()).foldLeft(z)(f)
    }
  }
}

def splitter[A]: Splitter[A] // on every collection


trait Builder[A, Repr] {
  def +=(elem: A): Builder[A, Repr]

  def result: Repr
}

def newBuilder[A, Repr]: Builder[A, Repr] // on every collection of type A


trait Traversable[T] {
  def foreach(f: T => Unit): Unit

  def newBuilder: Builder[T, Traversable[T]]

    def filter(p: T => Boolean) = {
      val b = newBuilder
      foreach(t => {
        if (p(t)) {
          b.+=(t)
        }
      })
      b.result
    }

  def newCombiner: Combiner[T, Traversable[T]]

  def splitter: Splitter[Traversable[T]]

  def filterUsingSplitterAndCombiner(p: T => Boolean) = {
    if (splitter.remaining < threshold) {
      this.filter(p)
    }
    else {
      val children = for (child <- splitter.split) yield task {

      }
      children.map(_.join())
    }
  }
}

trait Combiner[A, Repr] extends Builder[A, Repr] {
  def combine(that: Combiner[A, Repr]): Combiner[A, Repr]
}

def newCombiner[A, Repr]: Combiner[A, Repr] // defined on every collection of type A