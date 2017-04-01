package week3

object intsets {
	Empty                                     //> res0: week3.Empty.type = .
	val t1 = new NonEmpty(3)                  //> t1  : week3.NonEmpty = {.3.}
	val t2 = t1 include 4                     //> t2  : week3.IntSet = {.3{.4.}}
	val t3 = new NonEmpty(5)                  //> t3  : week3.NonEmpty = {.5.}
	val t4 = t2 union t3                      //> t4  : week3.IntSet = {{{.3.}4.}5.}
}


abstract class IntSet {
  def contains(x: Int): Boolean
  def include(x: Int): IntSet
  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  override def contains(x: Int): Boolean = false
	override def include(x: Int): IntSet = new NonEmpty(x)
	override def union(other: IntSet): IntSet = other
	override def toString = "."
}

class NonEmpty(element: Int, left: IntSet, right: IntSet) extends IntSet {

	def this(element: Int) = this(element, Empty, Empty)
	
	override def contains(x: Int): Boolean =
		if (x < element) left contains x
		else if (x > element) right contains x
		else true
		
	override def include(x: Int): IntSet =
		if (x < element) new NonEmpty(element, left include x, right)
		else if (x > element) new NonEmpty(element, left, right include x)
		else this
		
	override def union(other: IntSet): IntSet =
		((left union right) union other) include element
		
	override def toString = "{" + left + element + right + "}"
}