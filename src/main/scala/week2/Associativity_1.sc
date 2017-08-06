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


val a = Array(3, 6, 5, 8, 7)

val sum = reduce[Int](a, _ + _)
val multiply = map(a, (x: Int) => x * x)

def power(x: Int, p: Int) = Math.pow(Math.abs(x), p)
val p = 2


reduce(map(a, power(_, p)), (x: Int, y: Int) => x + y)


// floating point addition is not associative
val e = 1e-200
val x = 1e200
val mx = -x

(e + x) + mx == e + (x + mx)

// floating point multiplication is communicative but not associative
e * x * mx == e * (x * mx)
