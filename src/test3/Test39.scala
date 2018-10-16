package test3

import test3.List.append
import test3.List.reversal

/**
  * 练习3.9 到 3.24
  */
object Test39 {

  def length2[A](l: List[A]): Int = {
    foldLeft(l, 0)((size, _) => size + 1)
  }

  def foldRight1[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reversal(as), z)((b, a) => f(a, b))
  }

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(as, (b: B) => b)((g, a) => b => g(f(a, b)))(z)
  }

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
    }
  }

  def f0(x: Int, y: Int): Int = {
    println(x + "\t" + y)
    x + y
  }

  def z0: Int => Int = (b: Int) => b

  def f1: (Int => Int, Int) => Int => Int = {
    (g, a) => b => g(f0(a, b))
  }
  def appendLeft[A](l1:List[A],l2:List[A]):List[A]={
    foldLeft(l1,l2)((tail,a)=>Cons(a,tail))
  }

  def main(args: Array[String]): Unit = {
    val list = List(1, 2, 3, 4, 5, 6)
    val l2 = List(101, 201, 301, 401, 501, 106)
    println(length2(list))

    val function = foldLeft(list, z0)(f1)
    println(function(0))
    println(foldLeft(list, 0)(f0))
    println(foldRight1(list, 0)(f0))
    println(foldRight2(list, 0)((x, y) => x * y))
    println(reversal(list))
    println(append(list,l2))
    println(appendLeft(l2,list))
  }
}
