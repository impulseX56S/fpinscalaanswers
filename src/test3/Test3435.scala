package test3

import test3.Test3233.tail

object Test3435 {

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n > 0) drop(tail(l), n - 1)
    else if (n == 0) l
    else Nil
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(a, tail) => if (f(a)) dropWhile(tail, f) else Cons(a, dropWhile(tail, f))
      case _ => l
    }
  }

  // else Cons(a, dropWhile(tail, f))
  def isLast[A](l: List[A]): Boolean = {
    l match {
      case Cons(_, Nil) => true
      case _ => false
    }
  }

  def init[A](l: List[A]): List[A] = {

    l match {
      case Nil => sys.error("init of empty list")
      case Cons(head, tail) => if (isLast(tail)) Cons(head, Nil) else Cons(head, init(tail))
    }
  }

  def main(args: Array[String]): Unit = {
    val list = List(1, 2, 3, 4, 5)
    val tailList = drop(list, 3)
    val filterList = dropWhile(list, (x: Int) => x > 3)
    println(tailList)
    println(filterList)
    println(init(list))
  }
}
