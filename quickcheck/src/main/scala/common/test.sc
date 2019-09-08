package common

import quickcheck._;

object test {
  val x = new QuickCheckHeap with quickcheck.test.Bogus4BinomialHeap
                                                  //> x  : quickcheck.QuickCheckHeap with quickcheck.test.Bogus4BinomialHeap = Pro
                                                  //| p
  val y = new QuickCheckHeap with quickcheck.test.BinomialHeap
                                                  //> y  : quickcheck.QuickCheckHeap with quickcheck.test.BinomialHeap = Prop
  import y._
  val heap1 = insert(3, empty)                    //> heap1  : common.test.y.H = List(Node(3,0,List()))
  val heap2 = insert(4, heap1)                    //> heap2  : common.test.y.H = List(Node(3,1,List(Node(4,0,List()))))
  val heap3 = insert(5, heap2)                    //> heap3  : common.test.y.H = List(Node(5,0,List()), Node(3,1,List(Node(4,0,Lis
                                                  //| t()))))
  val heap4 = insert(6, heap3)                    //> heap4  : common.test.y.H = List(Node(3,2,List(Node(5,1,List(Node(6,0,List())
                                                  //| )), Node(4,0,List()))))
  deleteMin(heap3)                                //> res0: common.test.y.H = List(Node(4,1,List(Node(5,0,List()))))
  heap3(0).c                                      //> res1: List[common.test.y.Node] = List()

  def findAndDeleteAll(h: H, acc: List[A]): List[A] =
    if (isEmpty(h)) acc
    else {
      val m = findMin(h)
      findAndDeleteAll(deleteMin(h), acc :+ m)
    }                                             //> findAndDeleteAll: (h: common.test.y.H, acc: List[common.test.y.A])List[commo
                                                  //| n.test.y.A]

  findAndDeleteAll(heap3, Nil)                    //> res2: List[common.test.y.A] = List(3, 4, 5)

  heap3 == heap3                                  //> res3: Boolean = true

  def fromList(l: List[A], h: H): H = l match {
    case Nil     => h
    case x :: xs => fromList(xs, insert(x, h))
  }                                               //> fromList: (l: List[common.test.y.A], h: common.test.y.H)common.test.y.H
  
  val h = fromList(List(0, -1), empty)            //> h  : common.test.y.H = List(Node(-1,1,List(Node(0,0,List()))))
  findAndDeleteAll(h, Nil)                        //> res4: List[common.test.y.A] = List(-1, 0)

}