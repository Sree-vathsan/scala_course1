package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}

  /*test("test someText") {
    new TestTrees {
      assert(createCodeTree("someText".toList).toString() === t1)
      //assert(1 == 1)
    }
  }          */




  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("times of a char list a-2") {
    new TestTrees {
      assert(List(('a',2)) === times(List('a', 'a')))
    }
  }

  test("times of a char list -a2b1") {
    new TestTrees {
      assert(List(('a',2), ('b',1)) === times(List('a', 'b', 'a')))
    }
  }

  test("times of a char list -Hello") {
    new TestTrees {
      assert(List(('H',1), ('e',1), ('l', 2), ('o', 1)) === times("Hello".toList).reverse)
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton tree contains only one CodeTree") {
    assert(!singleton(List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4))))
    assert(singleton(List(Leaf('x',4))))
    assert(!singleton(List()))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  /*test("until all leaves are merged into single") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val retTree = until(singleton, combine)(leaflist)
    assert(retTree.length==1)
  }*/
  test("encode a very short text with frenchCode") {
        new TestTrees {
          assert(encode(frenchCode)("sdx".toList) === List(0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0))
        }
  }

  test("encode a very short text with t1") {
       new TestTrees {
          assert(encode(t1)("ab".toList) === List(0,1))
       }
  }

  test("decode using french Code tree") {
    new TestTrees {
      assert(decode(frenchCode,
        List(1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1)) === List('e', 'n', 'c', 'o', 'r', 'e', 'u', 'n', 't', 'e', 'x', 't', 'e', 't', 'r', 'e', 's', 's', 'e', 'c', 'r'))
    }
  }

  test("decode and encode long seq should be identity") {
    val l1 = List(1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1)
    new TestTrees {
      val l2 = quickEncode(frenchCode)(decode(frenchCode, l1))
       assert(l1.length == l2.length)
     }
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("quick encode long text") {
    val l1 = List('t', 'u', 'r', 'e', ' ', 'f', 'r', 'o', 'm', ' ', '4', '5', ' ', 'B', 'C', ',', ' ', 'm', 'a', 'k', 'i', 'n', 'g', ' ', 'i', 't', ' ', 'o', 'v', 'e', 'r', ' ', '2', '0', '0', '0', ' ', 'y', 'e', 'a', 'r', 's', ' ', 'o', 'l', 'd', '.', ' ', 'R', 'i', 'c', 'h', 'a', 'r', 'd', ' ', 'M', 'c')
    println(l1)
    println(encode(frenchCode)(l1))
    println(quickEncode(frenchCode)(l1))
    //println(decode(frenchCode, encode(frenchCode)(l1)))
    new TestTrees {
      assert(encode(frenchCode)(l1) === quickEncode(frenchCode)(l1))
    }
  }
}
