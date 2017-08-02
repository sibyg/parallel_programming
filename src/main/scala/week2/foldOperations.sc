import common._

List(1, 3, 8).fold(100)((s, x) => s + x)
List(1, 3, 8).foldLeft(100)((s, x) => s - x)
List(1, 3, 8).foldRight(100)((s, x) => s - x)
List(1, 3, 8).reduceLeft((s, x) => s - x)
List(1, 3, 8).reduceRight((s, x) => s - x)

sealed abstract class Tree[A]

case class Leaf[A](value: A) extends Tree[A]

case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]

def reduce[A](t: Tree[A], f: (A, A) => A): A = t match {
  case Leaf(v) => v
  case Node(l, r) => f(parallel(reduce[A](l, f), reduce[A](r, f)))
}


def tree = Node(Leaf(1), Node(Leaf(3), Leaf(8)))
def fMinus = (x: Int, y: Int) => x - y
def res = reduce(tree, fMinus)
res


def toList[A](t: Tree[A]): List[A] = t match {
  case Leaf(v) => List(v)
  case Node(l, r) => toList[A](l) ++ toList[A](r)
}

def map[A, B](t: Tree[A], f: A => B): Tree[B] = {
  case Leaf(v) => Leaf(f(v))
  case Node(l, r) => Node(map(l, f), map(r, f))
}

//toList(tree) == reduce(map(tree, List(_)), _ ++ _)
