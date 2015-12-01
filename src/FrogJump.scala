
object FrogJump extends App{
    def solution(X: Int, Y: Int, D: Int): Int = {
      var count = 0
      if (X==Y){return 0}
      var currantjump = X
      while(currantjump < Y){
        currantjump = currantjump + D
        count =   count + 1
      }
      count
    }

  val test1 = solution(1,5,2)
  println(test1)
}
