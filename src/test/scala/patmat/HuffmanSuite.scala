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

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }
  
  test("makeOrderedLeafList for empty frequency table") {
    assert(makeOrderedLeafList(List()) === List())
  }
  
  test("makeOrderedLeafList for a single frequency table") {
    assert(makeOrderedLeafList(List(('t', 2))) === List(Leaf('t',2)))
  }

  test("makeOrderedLeafList for some ordered frequency table") {
    assert(makeOrderedLeafList(List(('t', 1), ('e', 2), ('x', 3))) === List(Leaf('t',1), Leaf('e',2), Leaf('x',3)))
  }
  
  test("makeOrderedLeafList for some reverse ordered frequency table") {
    assert(makeOrderedLeafList(List(('t', 3), ('e', 2), ('x', 1))) === List(Leaf('x',1), Leaf('e',2), Leaf('t',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
  
  test("singleton for empty list") {
    new TestTrees {
      assert(singleton(List()) == false)
    }
  }
  
  test("singleton for a 1-elements list") {
    new TestTrees {
      assert(singleton(List(new Fork(new Leaf('a', 1), new Leaf('b', 1), List('a', 'b'), 2))) == true)
    }
  }
  
  test("singleton for a 2-elements list") {
    new TestTrees {
      val e1 = new Fork(new Leaf('a', 1), new Leaf('b', 1), List('a', 'b'), 2);
      val e2 = new Fork(new Leaf('a', 1), new Leaf('b', 1), List('a', 'b'), 2);
      assert(singleton(List(e1, e2)) == false)
    }
  }
  
  test("singleton for a 3-elements list") {
    new TestTrees {
      val e1 = new Fork(new Leaf('a', 1), new Leaf('b', 1), List('a', 'b'), 2);
      assert(singleton(List(e1, e1, e1)) == false)
    }
  }
  
  test("times for empty list") {
    new TestTrees {
      assert(times(List()) == List())
    }
  }
  
  test("times for 1 elem") {
    new TestTrees {
      assert(times(List('a')) == List(('a', 1)))
    }
  }
  
  test("times for 1 elem two times") {
    new TestTrees {
      val actual = times(List('a', 'a'))
      assert(actual == List(('a', 2)))
    }
  }
  
  test("times for 1 elem three times") {
    new TestTrees {
      assert(times(List('a', 'a', 'a')) == List(('a', 3)))
    }
  }

  test("times for 2 elem") {
    new TestTrees {
      val actual = times(List('a', 'b'))
//      println("times for 2 elem: times(List('a', 'b')) =>" + actual);
      assert(actual == List(('a', 1), ('b', 1)) || actual == List(('b', 1), ('a', 1)))
    }
  }

  test("times for 2 elem multiple times") {
    new TestTrees {
      val actual = times(List('a', 'b', 'b', 'b', 'a'))
//      println("times for 2 elem multiple times: times(List('a', 'b', 'b', 'b', 'a')) =>" + actual);
      assert(actual == List(('a', 2), ('b', 3)) || actual == List(('b', 3), ('a', 2)))
    }
  }
  
  test("createCodeTree with empty list") {
    new TestTrees {
      intercept[IllegalArgumentException] {
	   createCodeTree(List())
	  }
    }
  }
  
  test("createCodeTree with 1-elem list") {
    new TestTrees {
      val actual = createCodeTree(List('a'))
//      println("times for 2 elem multiple times: times(List('a', 'b', 'b', 'b', 'a')) =>" + actual);
      assert(actual == new Leaf('a', 1))
    }
  }
  
  test("createCodeTree with non-empty list") {
    new TestTrees {
      val actual = createCodeTree(List('a', 'b', 'b', 'b', 'a', 'c'))
//      println("createCodeTree(List('a', 'b', 'b', 'b', 'a', 'c')) =>" + actual);
      assert(actual == Fork(Leaf('b',3),Fork(Leaf('c',1),Leaf('a',2),List('c', 'a'),3),List('b', 'c', 'a'),6))
    }
  }
  
  test("decodedSecret") {
    new TestTrees {
      val actual = decodedSecret
//      println("decodedSecret =>" + actual);
      assert(actual == string2Chars("huffmanestcool"))
    }
  }

  test("encode") {
    new TestTrees {
      val actual = encode(frenchCode)(string2Chars("huffmanestcool"))
//      println("decodedSecret =>" + actual);
      assert(actual == secret)
    }
  }

  test("codeBits") {
    new TestTrees {
      val table = List(('c', List(1, 1, 1)), ('d', List(1, 1, 0)), ('f', List(1, 0, 1)))
      val actual = codeBits(table)('d')
      assert(actual == List(1, 1, 0))
    }
  }

  test("quickEncode") {
    new TestTrees {
      val slow = encode(frenchCode)(string2Chars("huffmanestcool"))
      val actual = quickEncode(frenchCode)(string2Chars("huffmanestcool"))
//      println("decodedSecret =>" + actual);
      assert(slow == actual)
    }
  }
}
