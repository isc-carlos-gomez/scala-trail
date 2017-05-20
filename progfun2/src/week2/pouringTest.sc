package week2

object pouringTest {
	val problem = new Pouring(Vector(4, 9, 19))
                                                  //> problem  : week2.Pouring = week2.Pouring@6be46e8f
	problem.moves                             //> res0: scala.collection.immutable.IndexedSeq[Product with Serializable with we
                                                  //| ek2.pouringTest.problem.Move] = Vector(Empty(0), Empty(1), Empty(2), Fill(0),
                                                  //|  Fill(1), Fill(2), Pour(0,1), Pour(0,2), Pour(1,0), Pour(1,2), Pour(2,0), Pou
                                                  //| r(2,1))
  problem.solutions(17)                           //> res1: Stream[week2.pouringTest.problem.Path] = Stream(Fill(0) Pour(0,2) Fill
                                                  //| (1) Pour(1,2) Fill(0) Pour(0,2)--> Vector(0, 0, 17), ?)

}