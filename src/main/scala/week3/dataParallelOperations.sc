(1 until 1000).par.filter(n => n % 3 == 0).count(n => n.toString == n.toString.reverse)

def sum(xs: Array[Int]): Int = {
  xs.par.foldLeft(0)(_ + _)
}

def fold[A](z: A)(f: (A, A) => A): A

def sumUsingFold(xs: Array[Int]): Int = {
  xs.par.fold(0)(_ + _)
}

def max(xs: Array[Int]): Int = {
  xs.par.fold(Int.MinValue)(math.max)
}

/**
  * Lessons learned -
  * 1. Applying a function on the neutral element and other element of a collection
  * is commutative - f(z,x1) == f(x1,z)
  * 2. function is associative on all elements of collection i.e
  * f(f(x1,x2),x3) == f(x1 f(fx2,x3))
  */

/**
  * paper wins over rock
  * rock wins over scissors
  * scissors wins over paper
  */
object Game extends Enumeration {
  val Paper, Rock, Scissor, None = Value
}

Array(Game.Paper, Game.Rock,
  Game.Paper, Game.Scissor).par.fold(Game.None)(play)

def play(player1: Game.Value, player2: Game.Value) = List(player1, player2).sorted match {
  case List(Game.Paper, Game.Rock) => Game.Paper
  case List(Game.Rock, Game.Scissor) => Game.Rock
  case List(Game.Scissor, Game.Paper) => Game.Scissor
  case List(a, b) if a == b => a
  case List(Game.None, a) => a
}

def isVowel(c: Char) = Array('A', 'E', 'I', 'O', 'U').contains(Character.toUpperCase(c))

// doesn't compile as fold operation expects all input and output types to be same!!!
//Array('E', 'P', 'F', 'L').par.fold(0)((count, c) => if (isVowel(c)) count + 1 else count)

/**
  * custom definition of aggregate
  * aggregate function helping in parallel operation
  * by supporting 2 functions, one operating on elements
  * and the other operating on accumulator
  * @param z : initial value of accumulator
  * @param f : function1 operating on element and accumulator returning new accumulator
  * @param g : function2 operating on elements on the collection
  * @tparam A: type of collection
  * @tparam B: type of accumulator
  * @return accumulator
  */
def aggregate[A, B](z: B)(f: (B, A)=> B)(g: (B, B)=> B): B


Array('E', 'P', 'F', 'L').par.aggregate(0)( // aggregate from the library
  (count, c) => if (isVowel(c)) count + 1 else count, _ + _)

// parallel ops supported by map, filter, flatMap, groupBy