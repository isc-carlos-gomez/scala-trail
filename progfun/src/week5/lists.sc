package week5

object lists {

  def init[T](xs: List[T]): List[T] = xs match {
    case List()  => throw new Error("init of empty list")
    case List(x) => Nil
    case y :: ys => y :: init(ys)
  }                                               //> init: [T](xs: List[T])List[T]

  init(List(1, 2))                                //> res0: List[Int] = List(1)

  def removeAt[T](n: Int, xs: List[T]): List[T] = xs match {
    case List() => xs
    case y :: ys =>
      if (n == 0)
        ys
      else
        y :: removeAt(n - 1, ys)
  }                                               //> removeAt: [T](n: Int, xs: List[T])List[T]

  removeAt(1, List('a', 'b', 'c', 'd'))           //> res1: List[Char] = List(a, c, d)

  def flatten(xs: List[Any]): List[Any] = xs match {
    case (y: List[Any]) :: ys => flatten(y) ++ flatten(ys)
    case y :: ys              => y :: flatten(ys)
    case Nil                  => Nil
  }                                               //> flatten: (xs: List[Any])List[Any]

  flatten(List(List(1, 1), 2, List(3, List(5, 8))))
                                                  //> res2: List[Any] = List(1, 1, 2, 3, 5, 8)
  def squareList(xs: List[Int]): List[Int] =
    xs match {
      case Nil     => xs
      case y :: ys => y * y :: squareList(ys)
    }                                             //> squareList: (xs: List[Int])List[Int]

  def squareList2(xs: List[Int]): List[Int] =
    xs map (x => x * x)                           //> squareList2: (xs: List[Int])List[Int]

  squareList(List(2, 5))                          //> res3: List[Int] = List(4, 25)
  squareList2(List(2, 5))                         //> res4: List[Int] = List(4, 25)

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil      => Nil
    case x :: xs1 => {
    	val (g, r) = xs span (y => y == x)
    	g :: pack(r)
    }
  }                                               //> pack: [T](xs: List[T])List[List[T]]
 
  pack(List("a", "a", "a", "b", "c", "c", "a"))   //> res5: List[List[String]] = List(List(a, a, a), List(b), List(c, c), List(a)
                                                  //| )
  
  def encode[T](xs: List[T]): List[(T, Int)] =
  	pack(xs) map (ys => (ys.head, ys.length)) //> encode: [T](xs: List[T])List[(T, Int)]
  	
  encode(List("a", "a", "a", "b", "c", "c", "a")) //> res6: List[(String, Int)] = List((a,3), (b,1), (c,2), (a,1))
}