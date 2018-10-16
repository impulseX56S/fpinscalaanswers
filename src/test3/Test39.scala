package test3

import test3.List.length
import test3.List.foldLeft

object Test39 {

  def length2[A](l: List[A]): Int = {
    foldLeft(l,0)((size,_)=> size+1)
  }

  def main(args: Array[String]): Unit = {
    val list = List(1, 2, 3, 4, 5,6)
    println(length2(list))
    println(foldLeft(list, 0)((x, y) => x + y))
  }
}
