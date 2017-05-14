package week1

object session {
	def f(n: Int) = {
		for {
			x <- 2 to n
			y <- 2 to x
			if (x % y == 0)
		} yield (x, y)
	}                                         //> f: (n: Int)scala.collection.immutable.IndexedSeq[(Int, Int)]
	
	def g(n: Int) = {
		(2 to n) flatMap (x =>
			(2 to x) withFilter (y =>
				x % y == 0) map (y => (x, y)))
	}                                         //> g: (n: Int)scala.collection.immutable.IndexedSeq[(Int, Int)]
	
	f(6) == g(6)                              //> res0: Boolean = true

}