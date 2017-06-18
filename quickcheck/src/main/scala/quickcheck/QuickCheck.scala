package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      x <- arbitrary[Int]
      h <- genHeap
    } yield insert(x, h))
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("findMin") = forAll { (x: Int, y: Int) =>
    val m = x min y
    findMin(insert(x, insert(y, empty))) == m
  }

  property("deleteMin(singleton heap) 	= empty heap") = forAll { (x: Int) =>
    isEmpty(deleteMin(insert(x, empty)))
  }

  property("findMin + deleteMin = ordered list") = {
    def findAndDeleteAll(h: H, acc: List[A]): List[A] =
      if (isEmpty(h)) acc
      else {
        val m = findMin(h)
        findAndDeleteAll(deleteMin(h), acc :+ m)
      }
    forAll { h: H =>
      val elems = findAndDeleteAll(h, Nil)
      elems == elems.sorted
    }
  }

  property("findMin on meld h1 and h2 = findMin on h1 or findMin on h2") =
    forAll { (h1: H, h2: H) =>
      (!isEmpty(h1) && !isEmpty(h2)) ==> {
        val m = findMin(meld(h1, h2))
        m == findMin(h1) || m == findMin(h2)
      }
    }

  property("meld(h, empty) = h") = forAll { h: H =>
    meld(h, empty) == h
  }

  property("insert in one heap and then meld == meld and then insert") =
    forAll { (h1: H, h2: H, x: Int) =>
      findMin(meld(insert(x, h1), h2)) == findMin(insert(x, meld(h1, h2)))
    }

  property("x") = {
    def fromList(l: List[A], h: H): H = l match {
      case Nil     => h
      case x :: xs => fromList(xs, insert(x, h))
    }
    def findAndDeleteAll(h: H, acc: List[A]): List[A] =
      if (isEmpty(h)) acc
      else {
        val m = findMin(h)
        findAndDeleteAll(deleteMin(h), acc :+ m)
      }
    forAll {
      (l: List[A]) =>
        val h = fromList(l, empty)
        l.sorted == findAndDeleteAll(h, Nil)
    }
  }

}
