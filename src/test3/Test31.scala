package test3

import test3.List.sum


object Test31 {
  def main(args: Array[String]): Unit = {
    val x = List[Int](1,2,3,4,5) match {
      case Cons(x,Cons(2,Cons(4,_))) =>x
      case Nil =>42
      case Cons(x,Cons(y,Cons(3,Cons(4,_)))) => x+y
      case _=> 101
      case Cons(h,t) => h+sum(t)
    }

    printf("ssss" +x)
  }


}
