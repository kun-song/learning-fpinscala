package com.satansk.fpinscala.monoids

import com.satansk.fpinscala.state.{RNG, State}
import com.satansk.fpinscala.testing.{Gen, Prop}
import org.scalatest.{Matchers, WordSpec}

/**
  * Author:  satansk
  * Email:   satansk@hotmail.com
  * Date:    18/1/5
  */
class MonoidSpec extends WordSpec with Matchers {
  import com.satansk.fpinscala.monoids.Monoid._

  "monoid law" should {
    "obey Associativity and Identity laws" in {

      val intGen = Gen.choose(-100, 100)
      val optionGen: Gen[Option[Int]] = Gen(State(RNG.int).map(i ⇒ if (i % 2 == 0) Some(i) else None))
      def intToIntGen: Gen[Int ⇒ Int] = intGen map (i ⇒ x ⇒ i + x)

      // intAddition
      Prop.run(monoidLaws(intAddition, intGen))

      // intMultiplication
      Prop.run(monoidLaws(intMultiplication, intGen))

      // booleanOr
      Prop.run(monoidLaws(booleanOr, Gen.boolean))

      // booleanAnd
      Prop.run(monoidLaws(booleanAnd, Gen.boolean))

      // optionMonoid
      Prop.run(monoidLaws(optionMonoid: Monoid[Option[Int]], optionGen))

      // This test failed, but I can't figure it out now.
      // endoMonoid
      Prop.run(monoidLaws(endoMonoid: Monoid[Int ⇒ Int], intToIntGen))

    }
  }

  "isSorted" should {

    "return true on empty list" in {
      isSorted(IndexedSeq()) shouldEqual true
    }

    "return true on list which has only one element" in {
      isSorted(IndexedSeq(1)) shouldEqual true
    }

    "return true on sorted list" in {
      isSorted(IndexedSeq(1, 2, 3)) shouldEqual true
    }

    "return false on unsorted list" in {
      isSorted(IndexedSeq(0, -1, 2, 3)) shouldEqual false
    }
  }

  "count" should {

    "return 0 on empty string" in {
      count("") shouldEqual 0
    }

    """return 1 on ("hello,")""" in {
      count("hello,") shouldEqual 1
    }

    """return 1 on (" hello ")""" in {
      count(" hello ") shouldEqual 1
    }

    """return 2 on (", hello world ")""" in {
      count(", hello world ") shouldEqual 3
    }
  }

  "bag" should {

    "return Map() on empty list" in {
      bag(IndexedSeq.empty[Int]) shouldEqual Map()
    }

    "works fine with non empty list" in {
      bag(IndexedSeq(1, 1, 2, 2, 3)) shouldEqual Map(1 → 2, 2 → 2, 3 → 1)
      bag(IndexedSeq("a", "a", "b")) shouldEqual Map("a" → 2, "b" → 1)
    }
  }

  "bag_2" should {

    "return Map() on empty list" in {
      bag_2(IndexedSeq.empty[Int]) shouldEqual Map()
    }

    "works fine with non empty list" in {
      bag_2(IndexedSeq(1, 1, 2, 2, 3)) shouldEqual Map(1 → 2, 2 → 2, 3 → 1)
      bag_2(IndexedSeq("a", "a", "b")) shouldEqual Map("a" → 2, "b" → 1)
    }
  }

}
