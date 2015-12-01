/*
* # Longest Valid Password
#
# A valid password has to contain at least one uppercase character and it cannot
# contain any digits. Given a non-empty string consisting on alphanumeric
# characters, write a function that reurns that maximum length of a substring
# that contains at least one uppercase letter.

* */



object StringPw extends App{
  def solution(S: String): Int = {
    val  i =0
    var   currantSubString=""
    var  currantLargestPw=0
    var currantPos =0
    def SubStringContainsInt(S:String):Boolean = {
      if(S.matches(".*\\d+.*")){
        true
      }
      else false
    }
    def stringContainUpperCase(S:String):Boolean = { S.exists(_.isUpper)}
    for (i <- 0 until S.length + 1){
      currantSubString =  S.substring(currantPos,i)
     // println(currantSubString)
      println("currant pos " +  currantPos)//Ok should remove
      //now with this subString check for conditions
      if (SubStringContainsInt(currantSubString) || !stringContainUpperCase(currantSubString)){
        println(SubStringContainsInt(currantSubString))
        println(stringContainUpperCase(currantSubString))
        println("was in here")
        println(currantSubString)
        currantPos = i
      }
   else {
        currantLargestPw = currantSubString.length
        println(currantLargestPw)
      }
    }
    1
  }
  solution("a0Ba")
}
