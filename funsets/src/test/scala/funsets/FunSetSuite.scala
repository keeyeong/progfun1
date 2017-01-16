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
  import scala.collection.immutable.Set

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
    * val s1 = singletonSet(1)
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
    val s4 = singletonSet(2)
    val s5 = singletonSet(-2)

    def set1(x: Int): Boolean = Set(1, 3, 4, 5, 7, 1000).contains(x)

    def set2(x: Int): Boolean = Set(1, 2, 3, 4).contains(x)

    def positiveSet(x: Int): Boolean = {
      x >= 0 && x <= 1000
    }

    def negativeSet(x: Int): Boolean = {
      x >= -1000 && x <= 0
    }

    def fullSet(x: Int): Boolean = {
      x >= -1000 && x <= 1000
    }

    def posiNegaTwoSet(x: Int): Boolean = {
      x == 2 || x == -2
    }

    def testFun1(x: Int): Int = x

    def testFun2(x: Int): Int = x * 2

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

  test("intersect only contains common elements of each set") {
    new TestSets {
      val s = intersect(s4, s2)
      assert(contains(s, 2), "Intersect 1")
      val ss = intersect(s, s1)
      assert(!contains(s, 1), "Intersect 2")
    }
  }

  test("diff of 2 singeltons") {
    new TestSets {
      val s = diff(s1, s2)
      assert(contains(s, 1), "diff 1")
    }
  }

  test("diff of 2 sets") {
    new TestSets {
      def s = diff(set1, set2)

      assert(contains(s, 5) && contains(s, 7) && contains(s, 1000), "diff 1")
    }
  }

  test("forall simple singleton test") {
    new TestSets {
      assert(forall(singletonSet(1), singletonSet(1)), "Forall 1 is 1")
      assert(forall(singletonSet(1), positiveSet), "Forall 1 is positive")
      assert(!forall(singletonSet(1), negativeSet), "Forall 1 is not negative")
      assert(forall(singletonSet(0), positiveSet), "Forall 0 is positive")
      assert(forall(singletonSet(0), positiveSet), "Forall 0 is negative")
    }
  }

  test("forall set of positive and negative 2 tests") {
    new TestSets {
      assert(forall(posiNegaTwoSet, fullSet), "Forall positive and negative 2 satisfy full")
      assert(!forall(posiNegaTwoSet, positiveSet), "Forall positive and negative 2 do not satisfy positive")
      assert(!forall(posiNegaTwoSet, negativeSet), "Forall positive and negative 2 do not satisfy negative")
    }
  }

  test("forall all in range test") {
    new TestSets {
      assert(forall(positiveSet, fullSet), "Forall positive is also part of full")
      assert(forall(negativeSet, fullSet), "Forall negative is also part of full")
    }
  }

  test("exists simple singleton test") {
    new TestSets {
      assert(exists(singletonSet(1), singletonSet(1)), "1 exists in 1")
      assert(exists(singletonSet(1), positiveSet), "1 exists in positive")
      assert(!exists(singletonSet(1), negativeSet), "1 not exists in negative")
      assert(exists(singletonSet(0), positiveSet), "0 exists in postive")
      assert(exists(singletonSet(0), positiveSet), "0 exists in negative")
    }
  }

  test("exists set of positive and negative 2 tests") {
    new TestSets {
      assert(exists(posiNegaTwoSet, fullSet), "Exists positive and negative 2 satisfy full")
      assert(exists(posiNegaTwoSet, positiveSet), "Exists positive and negative 2 satisfy positive")
      assert(exists(posiNegaTwoSet, negativeSet), "Exists positive and negative 2 satisfy negative")
    }
  }

  test("exists all in range test") {
    new TestSets {
      assert(exists(positiveSet, fullSet), "positive set exists in full set")
      assert(exists(negativeSet, fullSet), "negative set exists in full set")
    }
  }

  test("map simple singleton test") {
    new TestSets {
      assert(contains(map(singletonSet(1), testFun1), 1), "1 maps to 1")
      assert(contains(map(singletonSet(1), testFun2), 2), "1 maps to 2")
    }
  }

  test("map list test") {
    new TestSets {
      assert(contains(map(set1, testFun1), 1000), "mapped contains 1000*1")
      assert(contains(map(set1, testFun2), 2000), "mapped contains 1000*2")
    }
  }
}
