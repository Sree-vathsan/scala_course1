package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val s5 = singletonSet(5)
    val s7 = singletonSet(7)
    val s1000 = singletonSet(1000)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(!contains(s1, 99999), "Singleton")
      assert(contains(s2, 2), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains common elements between sets") {
    new TestSets {
      val s = union(s1, s2)
      val t = union(s2, s3)
      val sit = intersect(s, t)
      val tis = intersect(t, s)
      assert(contains(sit, 2))
      assert(!contains(sit, 1))
      assert(!contains(sit, 3))
      assert(contains(tis, 2))
      assert(!contains(tis, 1))
      assert(!contains(tis, 3))
    }
  }

  test("diff contains the set of all elements of `s` that are not in `t`") {
    new TestSets {
      val s = union(s1, s2)
      val t = union(s2, s3)
      val sdt = diff(s, t)
      val tds = diff(t, s)
      assert(!contains(sdt, 2))
      assert(contains(sdt, 1))
      assert(!contains(sdt, 3))
      assert(!contains(tds, 2))
      assert(!contains(tds, 1))
      assert(contains(tds, 3))
    }
  }

  test("filter the subset of `s` for which `p` holds") {
    new TestSets {
      val evenFilter = (x : Int) => (x%2==0)
      val oddFilter = (x: Int) => !evenFilter(x)
      val ef = filter(union(s1, s2), evenFilter)
      val of = filter(union(s1, s2), oddFilter)
      assert(contains(ef, 2))
      assert(!contains(ef, 1))
      assert(!contains(of, 2))
      assert(contains(of, 1))
    }
  }

  test("forAll whether all bounded integers within `s` satisfy `p`") {
    new TestSets {
      val evenFilter = (x : Int) => (x%2==0)
      val oddFilter = (x: Int) => !evenFilter(x)
      val lessThan5 = (x: Int) => x<5
      assert(forall(union(s1, s3), oddFilter))
      assert(!forall(union(s1, s3), evenFilter))
      assert(!forall(union(union(union(union(s1,s4),s5),s7),s1000),lessThan5))
    }
  }

  test("exists whether there exists a bounded integer within `s`") {
    new TestSets {
      val evenFilter = (x : Int) => (x%2==0)
      val oddFilter = (x: Int) => !evenFilter(x)
      assert(exists(union(s1, s2), oddFilter))
      assert(!forall(union(s1, s3), evenFilter))
    }
  }

  def makeSet(arr:Array[Int]):Set={
    var s:Set = singletonSet(arr(0));
    def iter(x:Int):Set= {
      if(x==arr.length) s;
      else {
        s = union(s, singletonSet(arr(1)));
        iter(x+1);
      }
    }
    iter(1);
  }

  test("MakeSet") {
    val arr = Array(1,2,3,4);
    print(arr);
    val s = makeSet(arr);
    printSet(s);
  }


  test("map Returns a set transformed by applying `f` to each element of `s`") {
    new TestSets {
      val reduceBy1 = (x : Int) => (x-1);
      val s14571000 = union(union(union(union(s1,s4),s5),s7),s1000);
      assert(contains(map(s14571000, reduceBy1),0))
      assert(contains(map(s14571000, reduceBy1),3))
      assert(contains(map(s14571000, reduceBy1),4))
      assert(contains(map(s14571000, reduceBy1),6))
      assert(contains(map(s14571000, reduceBy1),999))
      assert(!contains(map(s14571000, reduceBy1),-1))
      assert(!contains(map(s14571000, reduceBy1),1000))
    }
  }
}
