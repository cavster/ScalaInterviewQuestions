/**
 * Created by Colm on 30/11/2015.
 */
object Pangram {
//take ine a string make sure it contains all letters once

  def isPangram(strings: Array[String]): String = {
    var returnedString = ""
    strings.foreach(stringInArray =>
    if (stringInArray.toLowerCase.filter(x => x >= 'a' && x <= 'z').distinct.size == 26) returnedString =returnedString+"1"
    else
      returnedString =returnedString +"0"
    )
    returnedString
  }


}
