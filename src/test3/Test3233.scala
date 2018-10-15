package test3

object Test3233 {
    def tail[A](list:List[A]):List[A]={
      list match {
        case Nil => Nil
        case Cons(_,tail) =>tail
      }
    }
  def setHead[A](head:A,list:List[A]):List[A]={
    val tailList = tail(list)
    Cons(head,tailList)
  }

  def main(args: Array[String]): Unit = {
    val list = List(1,2,3,4,5)
    val tailList = tail(list)
    val newList = setHead(999,list)
    println(tailList)
    print(newList)
  }
}
