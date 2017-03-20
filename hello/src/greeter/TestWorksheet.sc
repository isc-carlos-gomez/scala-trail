package greeter

object TestWorksheet {
  val x = 1                                       //> x  : Int = 1
  def increase(i: Int) = i + 1                    //> increase: (i: Int)Int
  increase(x)                                     //> res0: Int = 2
  increase(x + 1)                                 //> res1: Int = 3
}