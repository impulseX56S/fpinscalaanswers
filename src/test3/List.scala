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
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
    }
  }

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, size) => size + 1)
  }

  def reversal[A](l: List[A]): List[A] = {
    foldLeft(l, Nil: List[A])((x, y) => Cons(y, x))
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def append[A](l1: List[A], l2: List[A]): List[A] = {
    foldRight(l1, l2)((a, tail) => Cons(a, tail))
  }

  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil: List[A])(append)
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = {
    as match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), map(t)(f))
    }
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    as match {
      case Nil => Nil
      case Cons(h, t) if f(h) => Cons(h, filter(t)(f))
      case Cons(h, t) if !f(h) => filter(t)(f)
    }
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    as match {
      case Nil => Nil
      case Cons(h, t) => append(f(h), flatMap(t)(f))
    }
  }

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  def addLists(as: List[Int], bs: List[Int]): List[Int] = {
    (as, bs) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addLists(t1, t2))
    }
  }

  def zipLists[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {
    (as, bs) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipLists(t1, t2)(f))
    }
  }
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @annotation.tailrec
    def isHead(l1: List[A], l2: List[A]): Boolean = {
      (l1, l2) match {
        case (_, Nil) => true
        case (Cons(h1, t1), Cons(h2, t2)) if (h1 == h2) => isHead(t1, t2)
        case _ => false
      }
    }
    sup match {
      case _ if isHead(sup, sub) => true
      case Nil => sub==Nil
      case Cons(_, t1)  => hasSubsequence(t1, sub)
    }
  }
}
