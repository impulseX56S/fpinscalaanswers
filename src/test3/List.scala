package test3

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }
  @annotation.tailrec
  def foldLeft[A,B](as:List[A],z:B)(f:(B,A)=>B):B={
    as match{
      case Nil => z
      case Cons(head,tail) =>foldLeft(tail,f(z,head))(f)
    }
  }
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, size) => size + 1)
  }
  def reversal[A](l:List[A]):List[A] ={
    foldLeft(l,Nil:List[A])((x,y)=>Cons(y,x))
  }
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def append[A](l1:List[A],l2:List[A]):List[A]={
    foldRight(l1,l2)((a,tail)=>Cons(a,tail))
  }
  def concat[A](l: List[List[A]]): List[A] ={
    foldRight(l, Nil:List[A])(append)
  }
}
