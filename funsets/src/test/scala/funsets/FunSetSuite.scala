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
   test("string take") {
     val message = "hello, world"
     assert(message.take(5) == "hello")
   }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
   test("adding ints") {
     assert(1 + 2 === 3)
   }


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

  test("intersect doesn't contain elements of any set") {
    new TestSets {
      val s = intersect(s1, s2)
      assert(!contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")
      assert(!contains(s, 3), "Intersect 3")
    }
  }

  test("intersect contains elements that are in both sets") {
    new TestSets {
      val s = intersect(s1, union(s1, s2))
      assert(contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")
      assert(!contains(s, 3), "Intersect 3")
    }
  }

  test("diff(s, t) contains all the elements of the set s that are not in the set t") {
    new TestSets {
      val s = union(s1, s2)
      val t = union(s2, s3)
      val r = diff(s, t)
      assert(contains(r, 1), "Diff 1")
      assert(!contains(r, 2), "Diff 2")
      assert(!contains(r, 3), "Diff 3")
    }
  }

  test("filter selects only the elements of a set that are accepted by a given predicate") {
    new TestSets {
      val s = union(union(s1, s2), s3)
      val r = filter(s, x => x <= 2)
      assert(contains(r, 1), "Diff 1")
      assert(contains(r, 2), "Diff 2")
      assert(!contains(r, 3), "Diff 3")
    }
  }

  test("forall predicate is true for all elements of the set") {
    new TestSets {
      val s = union(union(s1, s2), s3)
      assert(forall(s, x => x <= 3))
    }
  }

  test("forall predicate is false for one element") {
    new TestSets {
      val s = union(union(s1, s2), s3)
      assert(!forall(s, x => x <= 2))
    }
  }

  test("exists on a set that contains valid elements") {
    new TestSets {
      val s = union(union(s1, s2), s3)
      assert(exists(s, x => x <= 2))
    }
  }

  test("exists on a set that doesn't contain valid elements") {
    new TestSets {
      val s = union(union(s1, s2), s3)
      assert(!exists(s, x => x == 4))
    }
  }

  test("map transforms a given set into another one") {
    new TestSets {
      val s = union(s1, s2)
      val m = map(s, x => x + 1)
      assert(contains(s, 1), "s contains 1")
      assert(contains(s, 2), "s contains 2")
      assert(!contains(s, 3), "s doesn't contain 3")
      assert(!contains(s, 4), "s doesn't contain 4")
      assert(!contains(m, 1), "m doesn't contain 1")
      assert(contains(m, 2), "m contains 2")
      assert(contains(m, 3), "m contains 3")
      assert(!contains(m, 4), "m doesn't contain 4")
    }
  }

  test("map transforms a given set into a quadratic one") {
    new TestSets {
      val s = union(s1, s2)
      val m = map(s, x => x * x)
      assert(contains(s, 1), "s contains 1")
      assert(contains(s, 2), "s contains 2")
      assert(contains(m, 1), "m contains 1")
      assert(contains(m, 4), "m contains 4")
    }
  }

  test("map transforms a given set into a cubic one") {
    new TestSets {
      val s = union(s2, s3)
      val m = map(s, x => x * x * x + 1)
      assert(contains(s, 2), "s contains 2")
      assert(contains(s, 3), "s contains 3")
      assert(contains(m, 9), "m contains 9")
      assert(contains(m, 28), "m contains 28")
    }
  }

  test("map transforms a given set into a constant one") {
    new TestSets {
      val s = union(s2, s3)
      val m = map(s, x => 15)
      assert(contains(s, 2), "s contains 2")
      assert(contains(s, 3), "s contains 3")
      assert(contains(m, 15), "m contains 15")
      assert(!contains(m, 2), "m doesn't contain 2")
    }
  }

}
