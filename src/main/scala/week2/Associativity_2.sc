import scala.reflect.ClassTag

def reduce[A](a: Array[A], f: (A, A) => A): A = {
  var result: A = a(0)
  for (i <- 1 until a.length) {
    result = f(result, a(i))
  }
  result
}

def map[A, B](in: Array[A], f: A => B)(implicit tag: ClassTag[B]) = {
  val out: Array[B] = new Array[B](in.length)
  for (i <- in.indices) {
    out(i) = f(in(i))
  }
  out
}


val arr = Array(3, 7, 4, 8, 5)
def f(ab: (Int, Int), cd: (Int, Int)): (Int, Int) = {
  (ab._1 + cd._1, ab._2 + ab._2)
}

// get length and sum in one reduce
reduce(map(arr, (x: Int) => (x, 1)), f)
