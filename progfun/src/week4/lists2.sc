package week4

object lists2 {

  def isort(xs: List[Int]): List[Int] = xs match {
    case List()  => List()
    case y :: ys => insert(y, isort(ys))
  }                                               //> isort: (xs: List[Int])List[Int]

  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List()  => List(x)
    case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
  }                                               //> insert: (x: Int, xs: List[Int])List[Int]

  isort(List(7, 3, 9, 2))                         //> res0: List[Int] = List(2, 3, 7, 9)
  
  insert(7, isort(List(3, 9, 2)))                 //> res1: List[Int] = List(2, 3, 7, 9)
  insert(7, insert(3, isort(List(9, 2))))         //> res2: List[Int] = List(2, 3, 7, 9)
  insert(7, insert(3, insert(9, isort(List(2))))) //> res3: List[Int] = List(2, 3, 7, 9)
  insert(7, insert(3, insert(9, insert(2, isort(List())))))
                                                  //> res4: List[Int] = List(2, 3, 7, 9)
  insert(7, insert(3, insert(9, insert(2, List()))))
                                                  //> res5: List[Int] = List(2, 3, 7, 9)
  insert(7, insert(3, insert(9, List(2))))        //> res6: List[Int] = List(2, 3, 7, 9)
  insert(7, insert(3, 2 :: insert(9, List())))    //> res7: List[Int] = List(2, 3, 7, 9)
  //insert(7, insert(3, 2 :: List(9)))
  insert(7, insert(3, List(2, 9)))                //> res8: List[Int] = List(2, 3, 7, 9)
  insert(7, 2 :: insert(3, List(9)))              //> res9: List[Int] = List(2, 3, 7, 9)
  //insert(7, 2 :: 3 :: List(9))
  insert(7, List(2, 3, 9))                        //> res10: List[Int] = List(2, 3, 7, 9)
  2 :: insert(7, List(3, 9))                      //> res11: List[Int] = List(2, 3, 7, 9)
  2 :: 3 :: insert(7, List(9))                    //> res12: List[Int] = List(2, 3, 7, 9)
  //2 :: 3 :: 7 :: List(9)
  List(2, 3, 7, 9)                                //> res13: List[Int] = List(2, 3, 7, 9)
}