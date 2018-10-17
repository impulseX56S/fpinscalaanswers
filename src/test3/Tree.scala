package test3

sealed trait Tree[+A]

case object Nil extends Tree[Nothing]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left:Tree[A],right: Tree[A]) extends Tree[A]
object Tree {

  def apply[A](as:A*):Tree[A]={
    if (as.length == 1) Leaf(as.head)
    else  Branch(apply(as.head),apply(as.last))
  }

  def size[A](t:Tree[A]):Int={
    t match {
      case Leaf(_)=>1
      case Branch(l,r) =>size(l)+size(r)+1
    }
  }
  def maximun(t:Tree[Int]):Int={
    t match {
      case Leaf(a)=>a
      case Branch(l,r) =>maximun(l).max(maximun(r))
    }
  }
  def depth[A](t:Tree[A]):Int={
    t match {
      case Leaf(_)=>1
      case Branch(l,r) =>(depth(l)+1).max(depth(r)+1)
    }
  }
  def map[A,B](t:Tree[A])(f:A=>B):Tree[B]={
    t match {
      case Leaf(a)=>Leaf(f(a))
      case Branch(l,r) =>Branch(map(l)(f),map(r)(f))
    }
  }
  def fold[A,B](t:Tree[A])(g:A=>B)(f:(B,B)=>B):B={
    t match {
      case Leaf(a)=>g(a)
      case Branch(l,r) =>f(fold(l)(g)(f),fold(r)(g)(f))
    }
  }
  def sizeWithFold[A](t:Tree[A]):Int={
    fold(t)(_=>1)((a,b)=>a +b +1)
  }
  def maximunWithFold(t:Tree[Int]):Int={
    fold(t)(a=>a)((b,c)=> b.max(c))
  }
  def depthWithFold[A](t:Tree[A]):Int={
    fold(t)(_=>1)((a,b)=>(a+1).max(b+1))
  }
  def mapWithFold[A,B](t:Tree[A])(f:A=>B):Tree[B]={
    fold(t)(a => Tree(f(a)))((b,c)=>Branch(b,c) )
  }
}
