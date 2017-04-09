package week3

object lists {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  def nth[T](n: Int, list: List[T]): T =
    if (n < 0 || list.isEmpty) throw new IndexOutOfBoundsException
    else if (n == 0) list.head
    else nth(n - 1, list.tail)                    //> nth: [T](n: Int, list: week3.List[T])T

  val list = new Cons(1, new Cons(2, new Cons(3, Nil)))
                                                  //> list  : week3.Cons[Int] = 1, 2, 3, .

  nth(2, list)                                    //> res0: Int = 3
  //nth(-1, list)

  List()                                          //> res1: week3.List[Nothing] = .
  List(1)                                         //> res2: week3.List[Int] = 1, .
  List(1, 2)                                      //> res3: week3.List[Int] = 1, 2, .
}

trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty = false
  override def toString = head.toString() + ", " + tail
}

object Nil extends List[Nothing] {
  def isEmpty = true
  def head = throw new NoSuchElementException("Nil.head")
  def tail = throw new NoSuchElementException("Nil.tail")
  override def toString = "."
}

object List {
  def apply[T](): List[T] = Nil
  def apply[T](x1: T): List[T] = new Cons(x1, Nil)
  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, Nil))
}