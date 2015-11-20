/**
A non-empty zero-indexed array A consisting of N integers is given. Array A represents numbers on a tape.

Any integer P, such that 0 < P < N, splits this tape into two non-empty parts: A[0], A[1], ..., A[P ? 1] and A[P], A[P + 1], ..., A[N ? 1].

The difference between the two parts is the value of: |(A[0] + A[1] + ... + A[P ? 1]) ? (A[P] + A[P + 1] + ... + A[N ? 1])|

In other words, it is the absolute difference between the sum of the first part and the sum of the second part.

For example, consider array A such that:

  A[0] = 3
  A[1] = 1
  A[2] = 2
  A[3] = 4
  A[4] = 3
We can split this tape in four places:

P = 1, difference = |3 ? 10| = 7
P = 2, difference = |4 ? 9| = 5
P = 3, difference = |6 ? 7| = 1
P = 4, difference = |10 ? 3| = 7
Write a function:

object Solution { def solution(A: Array[Int]): Int }

that, given a non-empty zero-indexed array A of N integers, returns the minimal difference that can be achieved.

For example, given:

  A[0] = 3
  A[1] = 1
  A[2] = 2
  A[3] = 4
  A[4] = 3
the function should return 1, as explained above.

Assume that:

N is an integer within the range [2..100,000];
each element of array A is an integer within the range [?1,000..1,000].
Complexity:

expected worst-case time complexity is O(N);
expected worst-case space complexity is O(N), beyond input storage (not counting the storage required for input arguments).
Elements of input arrays can be modified.
 */
object TapeEquilibrium extends App{
  def solution(A: Array[Int]): Int ={
 //  frist of all need to find min need to loop through keep track of it //need to keep sliting and suming them up
    var i = 0
    var min = Int.MaxValue//this could work very unlikely to be this high find a way to clean
    for (i <- 1 until  A.length) {
      //at each point need to compare println frist then keep track
     val splitArray = A.splitAt(i)
      //I should split at frist spot ??
      println(" array 1 sumed" + splitArray._1.sum)
      println(" array 2 sumed " + splitArray._2.sum)
      //now get diff between two
      var diff = splitArray._1.sum - splitArray._2.sum
      println("Diff" + diff)
      if(diff < 0)
        diff = diff * -1
      if(diff < min)
        min = diff
      //keep the min //keep in mind neg may have to use condition statement
    }
    min
  }
  val testArray =  Array(3,1,2,4,3)
  val testArray2 =  Array(-3,1,-3,4,3)
  println("this is our answer " + solution(testArray2))
}
//Alterative solution found on stack overflow with recursion fancy!

object Solution extends App{
  def solution(A: Array[Int]): Int = {

    def diffAbs(a: Int, b: Int): Int = if (a - b < 0) b - a else a - b//Sloveing the neg problem better then mine...

    def findDiff(list: List[Int], leftSum: Int, rightSum: Int, diff: Int): Int = {
      list match {//ahh ok its 2 or less we get diff straigh away
        case x1 :: x2 :: xs =>
          val curDiff = diffAbs(leftSum, rightSum)
          val bestDiff = if (curDiff < diff) curDiff else diff
          println("this is x1 " + x1)
          println("this is x2 " + x2)
          println("this is xs " + xs)
          findDiff(list.tail, leftSum + x1, rightSum - x1, bestDiff)//then bestDiff is put back in clever!
        case _ => diff
      }
    }
    val leftSum: Int = A(0)
    println("This is left sum at start" + leftSum)
    val rightSum: Int = A.foldLeft(0)(_ + _) - A(0)
    val diff = diffAbs(leftSum, rightSum)//ok he does it here frist
    findDiff(A.toList.tail, leftSum, rightSum, diff)
  }
  val testArray =  Array(3,1,2,4,3)
  println("this is our answer " + solution(testArray))
}
import scala.math.{min, abs}

object Solution3 extends App {
  def solution(A: Array[Int]): Int = {
    if (A.size < 2 || A.size > 100000) sys.error(s"Invalid input - array size: ${A.size}")
    //good idea for checks
    val total = A.map(_.toLong).sum//gets total at start nessacary to do this way?

    A.foldLeft[(Int, Long, Long)](-1, -1, 0l) { (t, i) =>
        if (i < -1000 || i > 1000) sys.error(s"Invalid array element: $i")

        val (x, currentMin, lastLeftSum) = t
        val index = x + 1//since it starts at -1

        (index + 1 == A.size) match {
          case true =>
            //so if the array is do nothing
            // Do nothing on the last element
            t

          case false =>
            val leftSum = lastLeftSum.toLong + A(index).toLong//Keeps adding as we go along
            val rightSum = total - leftSum

            val thisMin = abs(leftSum- rightSum)
            val results = if (currentMin == -1) thisMin//could have this this for my problem
            else min(currentMin, thisMin)
            (index, results, leftSum)
            }        }

      }._2.toInt
  val testArray =  Array(3,1,2,4,3)
  println("this is our answer " + solution(testArray))
  //I found this solution harder to read and follow
}