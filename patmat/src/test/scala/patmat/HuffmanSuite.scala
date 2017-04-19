package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(t1, Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("weight of a leaf") {
    new TestTrees {
      assert(weight(Leaf('x', 8)) === 8)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("chars of a leaf") {
    new TestTrees {
      assert(chars(Leaf('x', 8)) === List('x'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }
  
  test("times of any string") {
    assert(times("hhhhellooo".toList) == List(('h', 4), ('e', 1), ('l', 2), ('o', 3)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("singleton of a singleton code tree") {
    assert(singleton(List(Leaf('x', 5))))
  }

  test("singleton of a long list of code tree") {
    assert(!singleton(List(Leaf('x', 5), Leaf('y', 8))))
  }

  test("singleton of an empty list") {
    assert(!singleton(List()))
  }

  test("decode secret") {
    assert(decodedSecret === "huffmanestcool".toList)
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("encode and decode a long text should be identity") {
    val text = "Hola mundo! como estas? a donde vas? por que?".toList
    val tree = createCodeTree(text)
    assert(decode(tree, encode(tree)(text)) === text)
  }

  test("merge simple code tables") {
    val table1 = List(('C', Nil))
    val table2 = List(('D', Nil))
    assert(mergeCodeTables(table1, table2) === List(('C', List(0)), ('D', List(1))))
  }

  test("merge complex code tables") {
    val table1 = List(('B', Nil))
    val table2 = List(('C', List(0)), ('D', List(1)))
    assert(mergeCodeTables(table1, table2) === List(('B', List(0)), ('C', List(1, 0)), ('D', List(1, 1))))
  }
  
  test("convert simple tree") {
    new TestTrees {
      assert(convert(t1) === List(('a',List(0)), ('b',List(1))))
    }
  }
  
  test("convert complex tree") {
    new TestTrees {
      assert(convert(t2) === List(('a',List(0, 0)), ('b',List(0, 1)), ('d',List(1))))
    }
  }
  
  test("quick encode and encode produce same result") {
    val text = "Hola mundo! como estas? a donde vas? por que?".toList
    val tree = createCodeTree(text)
    assert(quickEncode(tree)(text) === encode(tree)(text))
  }

}
