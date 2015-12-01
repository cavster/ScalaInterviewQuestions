


object ReverseString extends App{
//try linked list

  def linkList(l:List[Int],a:List[Int]):List[Int] = {
    l match {
      case Nil => a
      case x1 :: xs => {
        println("Id did enter here" + l)
        val newL = l.drop(0)
        linkList(newL, a :+ x1)
      }
    }
  }
  //assumeing there all differant use -=
    val test = List(0,1,2,3)
    val test2 = List(0)
    val test3 = linkList(test,test2)

  println(test3)
}

