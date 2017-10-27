package com.satansk.fpinscala.state

import org.scalatest.{Matchers, WordSpec}

/**
  * Author:  satansk
  * Email:   satansk@hotmail.com
  * Date:    17/10/22
  */
class StateSpec extends WordSpec with Matchers {
  import com.satansk.fpinscala.state.RNG._

  val rng = SimpleRNG(1)

  "SimpleRNG" should {
    "generates same random number given the same RNG" in {
      val (v1, _) = rng.nextInt
      val (v2, _) = rng.nextInt
      v1 shouldEqual v2
    }

    "generates different random numbers given different RNGs" in {
      val (v1, r1) = rng.nextInt
      val (v2, _) = r1.nextInt
      v1 should not equal v2
    }
  }

  "randomPairSame" should {
    "generates a pair with same value" in {
      val (v1, v2) = randomPairSame(rng)
      v1 shouldEqual v2
    }
  }

  "randomPair" should {
    "generates a pair with different values" in {
      val (v1, v2) = randomPair(rng)
      v1 should not equal v2
    }
  }

  "double" should {
    "generates double in [0, 1)" in {
      double(rng)._1 shouldEqual 1.7916224896907806E-4
    }
  }

  "ints" should {
    "generates a list of ints with the given number" in {
      ints(1)(rng)._1 shouldEqual List(384748)
      ints(2)(rng)._1 shouldEqual List(-1151252339, 384748)
      ints(3)(rng)._1 shouldEqual List(-549383847, -1151252339, 384748)
    }
  }

  "ints2" should {
    "generates a list of ints with the given number" in {
      ints2(1)(rng)._1 shouldEqual List(384748)
      ints2(2)(rng)._1 shouldEqual List(384748, -1151252339)
      ints2(3)(rng)._1 shouldEqual List(384748, -549383847, -1151252339)
    }
  }

  "doubleViaMap" should {
    "generates double in [0, 1)" in {
      doubleViaMap(rng)._1 shouldEqual 1.7916224896907806E-4
    }
  }

  "map2" should {
    "both" in {
      both(nonNegativeInt, double)(rng)._1 shouldEqual (384748, 0.5360936461947858)
    }
  }

  "intsViaSequence" should {
    "generates a list of ints with the given number" in {
      intsViaSequence(1)(rng)._1 shouldEqual List(384748)
      intsViaSequence(2)(rng)._1 shouldEqual List(-1151252339, 384748)
      intsViaSequence(3)(rng)._1 shouldEqual List(-549383847, -1151252339, 384748)
    }
  }

  "nonNegativeLessThan" should {
    "generates random numbers in [0, n)" in {
      nonNegativeLessThan(1)(rng)._1 shouldEqual 0
      nonNegativeLessThan(10)(rng)._1 shouldEqual 8
    }
  }
}
