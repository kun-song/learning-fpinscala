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

}
