package week2

object exercise {

  def reduce(combine: (Int, Int) => Int, map: Int => Int, zero: Int)(a: Int, b: Int): Int = {
    def loop(a: Int, accumulated: Int): Int =
      if (a > b)
        accumulated
      else
        loop(a + 1, combine(accumulated, map(a)))
    loop(a, zero)
  }                                               //> reduce: (combine: (Int, Int) => Int, map: Int => Int, zero: Int)(a: Int, b: 
                                                  //| Int)Int

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    reduce((x, y) => x * y, f, 1)(a, b)
  }                                               //> product: (f: Int => Int)(a: Int, b: Int)Int
  product(x => x * x)(3, 4)                       //> res0: Int = 144

  def factorial(n: Int): Int =
    product(x => x)(1, n)                         //> factorial: (n: Int)Int
  factorial(5)                                    //> res1: Int = 120

}