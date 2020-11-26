package funsets

import org.junit._

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite {

  import FunSets._

  @Test def `contains is implemented`: Unit = {
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
    val s6 = singletonSet(6)
    val s8 = singletonSet(8)
    val s10 = singletonSet(10)
    val s10001 = singletonSet(10001)
  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remvoe the
   * @Ignore annotation.
   */
  @Test def `singleton set one contains one`: Unit = {

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

  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  @Test def `intersection contains elements common in each set`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      val i = intersect(s, s1)
      assert(contains(i, 1), "Intersect 1")
      assert(!contains(i, 2), "Intersect 2")
    }
  }

  @Test def ` difference contains all elements of present in first set not in second set`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      val d = diff(s, s1)
      assert(contains(d, 2), "diff 2")
      assert(!contains(d, 1), "diff 1")
    }
  }

  @Test def `filter contains subset of given set for which predicate holds`: Unit = {
    new TestSets {
      val s = union(union(s1, s2),s4)
      val f = filter(s,(x:Int)=>x%2==0)
      assert(contains(f, 2), "filter 2")
      assert(contains(f, 4), "filter 4")
      assert(!contains(f, 1), "filter 1")
    }
  }

  @Test def `forall test to check if it satisfy p`: Unit = {
    new TestSets {
      val all_even = union(union(union(union(union(s2, s4),s4),s6),s8),s10)
      val all_not_even = union(union(union(union(union(s1, s4),s3),s6),s8),s10)
      def p(x:Int):Boolean = (x%2) == 0
      assert(forall(all_even,p), "forall all even")
      assert(!forall(all_not_even,p), "forall not all even")
    }
  }

  @Test def `exist test to check if it satisfy p`: Unit = {
    new TestSets {
      val all_even = union(union(union(union(union(s2, s4), s4), s6), s8), s10)
      val all_not_even = union(union(union(union(union(s1, s4), s3), s6), s8), s10)
      val odd = union(s1, s3)
      def p(x: Int): Boolean = (x % 2) == 0
      print(exists(odd, p))
      assert(exists(all_even, p), "exists all even")
      assert(exists(all_not_even, p), "exists not all even")
      assert(!exists(odd, p), "exists odd")

    }
  }

    @Test def `map test to check if it maps f`: Unit = {
      new TestSets {
        val s = union(union(union(union(union(s2, s4),s4),s6),s8),s10)
        def p(x: Int): Int = x + 1
        val new_s = map(s,p)
        assert(contains(new_s, 3),"map 3")
        assert(contains(new_s, 5),"map 5")
        assert(contains(new_s, 7),"map 7")
        assert(contains(new_s, 9),"map 9")
        assert(contains(new_s, 11),"map 11")
        assert(!contains(new_s, 2),"map 2")
        assert(!contains(new_s, 4),"map 4")
        assert(!contains(new_s, 6),"map 6")
        assert(!contains(new_s, 8),"map 8")
      }
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
