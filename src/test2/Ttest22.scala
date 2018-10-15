package test2

object Ttest22 {
  def isSorted[A](as:Array[A] ,ordered:(A,A) => Boolean) :Boolean ={
    var flag2= true
      var n = 0;
      while (n < as.length-1 && flag2)   {
        if (ordered(as(n),as(n+1))){
          n=n+1
        }else {
          flag2= false ;
        }
      }
    flag2
  }

  def sortfun1(a:Int,b:Int) :Boolean ={
    return a>b
  }
  def sortfun2(a:Int,b:Int) :Boolean ={
    return a<b
  }
  def printfun(array: Array[Int]):Unit={
    var n = 0;
    while(n < array.length){
      print(array(n) )
      print("\t")
      n =n+1
    }
  }
  def main(args: Array[String]): Unit = {
    val ints :Array[Int] = new Array[Int](5);
    ints(0)=3;ints(2)=13;ints(3)=3213;ints(1)=1;ints(4)=9;
    printfun(ints)
    println()
    val ints2 = ints.sortWith(sortfun1)
    printfun(ints2)
    println()
    val ints3 = ints.sortWith(sortfun2)
    printfun(ints3)
    println()
    println(isSorted[Int](ints,sortfun1))
    println(isSorted[Int](ints2,sortfun1))
    println(isSorted[Int](ints2,sortfun2))
    println(isSorted[Int](ints3,(x:Int,y:Int)=> x<y))
    println(isSorted[Int](ints3,(x:Int,y:Int)=> x>y ))
  }
}
