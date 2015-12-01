/*
http://stackoverflow.com/questions/30499166/how-to-know-if-an-array-can-be-sorted-by-one-swap-or-less
 */

//Q3 http://stackoverflow.com/questions/21635397/min-average-two-slice-codility

object SortedSwap extends App  {
  def solution(A: Array[Int]): Boolean = {
    if(isSorted(A)) return false
    def isSorted(xs: Array[Int]): Boolean = {
      var i = 1
      while (i < xs.length) {
        if (xs(i) < xs(i-1)) return false
        i += 1
      }
      true
    }
    var count = 0
    val sortedArray = A.sortWith(_<_)
    for(i <- 0 until  A.length ){
      if(!(A(i) == sortedArray(i))){
        count = count + 1
        println(count)
      }

    }
    true
  }
  val test1 = Array(1, 5, 3, 3, 7)
//1,3,3,5,7
  solution(test1)
//  println(solution(test1))
}
