import scala.collection.{GenSeq, GenSet, mutable}

/**
  * find the largest palindrome in the
  * given sequence of integers
  *
  * @param seq sequence of integers
  * @return largest palindrome
  */
def largestPalindrome(seq: GenSeq[Int]): Int = {
  seq.aggregate(Int.MinValue)(
    (largest, n) =>
      if (n > largest && n.toString == n.toString.reverse) n else largest
    , math.max
  )
}

var array = (1 to 1000000).toArray
largestPalindrome(array) //seq
largestPalindrome(array.par) //seq

