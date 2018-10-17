package test3


import test3.Tree._
object TestTree {
  def main(args: Array[String]): Unit = {
    val leaf1 =Tree(111)
    val leaf2= new Leaf(2)
    val leaf3 = new Leaf(13)
    val leaf4 = new Leaf(4)
    val leaf5 = new Leaf(5)
    val branch1 = new Branch(leaf1,leaf2)
    val branch2 = new Branch(leaf4,leaf5)
    val branch3 = new Branch(branch2,leaf3)
    val tree = new Branch(branch1,branch3)
    val t2 = new Branch(branch1,branch2)
    println(size(tree)+"\t"+sizeWithFold(tree))
    println(maximun(tree)+"\t"+maximunWithFold(tree))
    println(depth(tree)+"\t"+ depth(t2)  +"\t\t"+depthWithFold(tree)+"\t"+ depthWithFold(t2))
    println(map(tree)(a=>"="+a))
    println(mapWithFold(tree)(a=>"="+a))
  }
}
