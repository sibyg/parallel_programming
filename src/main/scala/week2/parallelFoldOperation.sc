sealed abstract class Tree[A]

case class Leaf[A](value: A) extends Tree[A]

case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

def reduce[A](t: Tree[A], f: (A, A) => A): A = t match {
  case Leaf(value) => value
  case Node(l, r) => f(reduce(l, f), reduce(r, f))
}


def map[A, B](t: Tree[A], f: A => B): Tree[B] = t match {
  case Leaf(v) => Leaf(f(v))
  case Node(l, r) => Node(map(l, f), map(r, f))
}

def reduce[A](a: Array[A], f: (A, A) => A): A = {
  var result: A = a(0)
  for (i <- 1 until a.length) {
    result = f(result, a(i))
  }
  result
}

def map[A, B](a: Array[A], f: A => B): Array[B] = {
  val result = new Array[B](a.length)
  for (i <- a.indices) {
    result(i) = f(a(i))
  }
  result
}

