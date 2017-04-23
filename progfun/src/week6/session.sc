package week6

object session {
  def isPrime(n: Int): Boolean =
    2 until n forall (n % _ != 0)                 //> isPrime: (n: Int)Boolean
  isPrime(7)                                      //> res0: Boolean = true
  isPrime(1567)                                   //> res1: Boolean = true
  isPrime(2525)                                   //> res2: Boolean = false

  def findPrimePairs(n: Int): Seq[(Int, Int)] =
    1 until n flatMap (i =>
      1 until i map (j =>
        (i, j))) filter { case (x, y) => isPrime(x + y) }
                                                  //> findPrimePairs: (n: Int)Seq[(Int, Int)]
  findPrimePairs(7)                               //> res3: Seq[(Int, Int)] = Vector((2,1), (3,2), (4,1), (4,3), (5,2), (6,1), (6,
                                                  //| 5))

  def findPrimePairs2(n: Int): Seq[(Int, Int)] =
    for {
      i <- 1 until n
      j <- 1 until i
      if isPrime(i + j)
    } yield (i, j)                                //> findPrimePairs2: (n: Int)Seq[(Int, Int)]

  findPrimePairs2(7)                              //> res4: Seq[(Int, Int)] = Vector((2,1), (3,2), (4,1), (4,3), (5,2), (6,1), (6,
                                                  //| 5))

  def scalarProduct(xs: List[Double], ys: List[Double]): Double =
    (for ((x, y) <- (xs zip ys)) yield x * y).sum //> scalarProduct: (xs: List[Double], ys: List[Double])Double

  scalarProduct(List(1.0, 2.0), List(3.0, 4.0))   //> res5: Double = 11.0
}