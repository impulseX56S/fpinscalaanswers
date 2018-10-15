object Mymodule{
  def fbnqsl(n0:Int):Int={
    val f1=0
    val f2=1
    def go(n:Int):Int={
      if (n ==1) f1
      else if (n == 2 ) f2
      else go(n-1) +go(n-2)
    }

   go(n0)
  }
  def fbnqsl2(n0:Int):Int={
    val f1=0
    val f2=1
    @annotation.tailrec
    def loop(a:Int,x:Int,y:Int):Int={
      if(a==n0) x
      else loop(a+1,x+y,x)
    }
    if (n0 ==1) f1
    else if (n0 == 2 ) f2
    else loop(2,f2,f1)
  }

  def main(args: Array[String]): Unit = {
    println(fbnqsl(4))
    println(fbnqsl(5))
    println(fbnqsl(6))
    println(fbnqsl2(7))
    println(fbnqsl2(8))
    println(fbnqsl2(9))
  }
}