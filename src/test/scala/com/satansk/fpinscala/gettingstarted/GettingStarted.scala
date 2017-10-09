package com.satansk.fpinscala.gettingstarted

import org.scalatest.{Matchers, WordSpec}

/**
  * Author:  satansk
  * Email:   satansk@hotmail.com
  * Date:    17/10/8
  */
class GettingStarted extends WordSpec with Matchers {
  import com.satansk.fpinscala.gettingstarted.MyModule._
  import com.satansk.fpinscala.gettingstarted.PolymorphicFunctions._

  "abs" should {
    "be able to handle negative number" in {
      abs(-2) shouldEqual 2
    }
    "be formatted" in {
      formatAbs(-2) shouldEqual "The absolute value of -2 is 2"
    }
  }

  "factorial" should {
    "be able to handle the factorial of 0" in {
      factorial(0) shouldEqual 1
    }
    "be able to handle the factorial of 30" in {
      factorial(30) shouldEqual 1409286144
    }
    "can't handle factorial of 100" in {
      factorial(100) shouldEqual 0
    }
  }

  "formatResult" should {
    "work fine with abs" in {
      formatResult("absolute value", -2, abs) shouldEqual "The absolute value of -2 is 2"
    }
    "work fine with factorial" in {
      formatResult("factorial", 30, factorial) shouldEqual "The factorial of 30 is 1409286144"
    }
    "work fine with (x: Int) => x + 1" in {
      formatResult("increment", 7, (x: Int) ⇒ x + 1) shouldEqual "The increment of 7 is 8"
    }
    "work fine with (x) => x + 1" in {
      formatResult("increment", 7, (x) ⇒ x + 1) shouldEqual "The increment of 7 is 8"
    }
    "work fine with x => x + 1" in {
      formatResult("increment", 7, x ⇒ x + 1) shouldEqual "The increment of 7 is 8"
    }
    "work fine with _ + 1" in {
      formatResult("increment", 7, _ + 1) shouldEqual "The increment of 7 is 8"
    }
    "work fine with x => { val r = x + 1; r }" in {
      formatResult("increment", 7, x ⇒ { val r = x + 1; r }) shouldEqual "The increment of 7 is 8"
    }
  }

  "fib" should {
    "return 0 for fib(0)" in {
      fib(0) shouldEqual 0
    }
    "return 1 for fib(1)" in {
      fib(1) shouldEqual 1
    }
    "return 1 for fib(2)" in {
      fib(2) shouldEqual 1
    }
    "return 5 for fib(5)" in {
      fib(5) shouldEqual 5
    }
  }

  "isSorted" should {
    "return true on empty array" in {
      isSorted[Int](Array.emptyIntArray, _ > _) shouldEqual true
    }
    "return true on array which contains only one element" in {
      isSorted[Int](Array(-10), _ > _) shouldEqual true
    }
    "return true on incremented array" in {
      isSorted[Int](Array(1, 2, 3, 4), _ > _) shouldEqual true
    }
    "return false on non incremented array" in {
      isSorted[Int](Array(1, 2, 3, 3), _ > _) shouldEqual false
    }
  }

  "partial1" should {
    "return partially applied function" in {
      partial1[Int, Int, Int](1, _ + _)(2) shouldEqual 3
    }
  }

  "curry" should {
    "return curried function" in {
      curry((x: Int, y: Int) ⇒ x + y)(1)(2) shouldEqual 3
    }
  }

  "uncurry" should {
    "return uncurried function" in {
      uncurry(curry((x: Int, y: Int) ⇒ x + y))(1, 2) shouldEqual 3
    }
  }

  "compose" should {
    "combine two function into one" in {
      compose((b: Int) ⇒ b * 2, (a: Int) ⇒ a + 3)(1) shouldEqual 8
    }
  }
}
