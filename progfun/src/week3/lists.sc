package week3

object lists {
	println("Welcome to the Scala worksheet") //> Welcome to the Scala worksheet
	
	def nth[T](n: Int, list: List[T]): T =
		if (n < 0 || list.isEmpty) throw new IndexOutOfBoundsException
		else if (n == 0) list.head
		else nth(n - 1, list.tail)        //> nth: [T](n: Int, list: week3.List[T])T
	
	val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))
                                                  //> list  : week3.Cons[Int] = 1, 2, 3, .
                                                  
	nth(2, list)                              //> res0: Int = 3
	nth(-1, list)                             //> java.lang.IndexOutOfBoundsException
                                                  //| 	at week3.lists$$anonfun$main$1.nth$1(week3.lists.scala:7)
                                                  //| 	at week3.lists$$anonfun$main$1.apply$mcV$sp(week3.lists.scala:14)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at week3.lists$.main(week3.lists.scala:3)
                                                  //| 	at week3.lists.main(week3.lists.scala)

}

trait List[T] {
	def isEmpty: Boolean
	def head: T
	def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
	override def isEmpty = false
	override def toString = head.toString() + ", " + tail
}

class Nil[T] extends List[T] {
	def isEmpty = true
	def head = throw new NoSuchElementException("Nil.head")
	def tail = throw new NoSuchElementException("Nil.tail")
	override def toString = "."
}