package com.satansk.fpinscala.testing

import com.satansk.fpinscala.state.{RNG, State}
import org.scalatest.{Matchers, WordSpec}

/**
  * Author:  satansk
  * Email:   satansk@hotmail.com
  * Date:    17/12/30
  */
class UsageSpec extends WordSpec with Matchers {
  import com.satansk.fpinscala.testing.Prop._
  import com.satansk.fpinscala.testing.Gen._

  "max" should {
    "get the biggest number of a list" in {
      val smallInt: Gen[Int] = Gen.choose(-10, 10)
      val maxProp = forAll(listOf1(smallInt))(xs ⇒ {
        val max = xs.max
        !xs.exists(_ > max)
      })

      Prop.run(maxProp)
    }
  }

  "sorted" should {
    "the sorted int list should have the same sum as the original list" in {
      val intGen = Gen.choose(-10, 10)
      val sortedProp = forAll(listOf(intGen))(xs ⇒ xs.sum == xs.sorted.sum)

      Prop.run(sortedProp)
    }
  }

  /**
    * Exercise 8.5 验证 takeWhile 的性质
    */
  "takeWhile" should {
    "prove 1" in {
      val isEven: Int ⇒ Boolean = _ % 2 == 0

      val p = Prop.forAll(Gen.listOf(Gen(State(RNG.int)))) {
        xs ⇒ xs.takeWhile(isEven).forall(isEven)
      }

      Prop.run(p)
    }
  }

}
