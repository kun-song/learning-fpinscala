package com.satansk.fpinscala.datastructures

import org.scalatest.{Matchers, WordSpec}

/**
  * Author:  satansk
  * Email:   satansk@hotmail.com
  * Date:    17/10/9
  */
class ListSpec extends WordSpec with Matchers {
  import com.satansk.fpinscala.datastructures.List._

  "sum" should {
    "return 0 on Nil" in {
      sum(Nil) shouldEqual 0
    }
    "work fine with non empty list" in {
      sum(List(1, 2, 3, 4, -1)) shouldEqual 9
    }
  }

  "product" should {
    "return 1.0 on Nil" in {
      product(Nil) shouldEqual 1.0
    }
    "return 0 for any list contains 0" in {
      product(List(1, 2, 3.0, 0)) shouldEqual 0
    }
    "return the product of list" in {
      product(List(1, 2.5, 3)) shouldEqual 7.5
    }
  }

  "tail" should {
    "throw NoSuchMethodException on empty list" in {
      intercept[NoSuchMethodException] {
        tail(Nil)
      }
    }
    "return all but the first element of a list" in {
      tail(List(1, 2, 3)) shouldEqual List(2, 3)
    }
  }

  "setHead" should {
    "throw NoSuchMethodException on empty list" in {
      intercept[NoSuchMethodException] {
        setHead(Nil, 0)
      }
    }
    "return a new list with the given head" in {
      setHead(List(1, 2, 3), 0) shouldEqual List(0, 2, 3)
    }
  }

  "drop" should {
    "return the original list if n is less or equal than 0" in {
      drop(List(1, 2, 3), 0) shouldEqual List(1, 2, 3)
      drop(List(1, 2, 3), -2) shouldEqual List(1, 2, 3)
    }
    "return Nil on empty list" in {
      drop(Nil, 10) shouldEqual Nil
    }
    "return all but the first n elements" in {
      drop(List(1, 2, 3, 4), 2) shouldEqual List(3, 4)
    }
  }

  "dropWhile" should {
    "drop the element until predicate get true" in {
      dropWhile[Int](List(1, 2, 3, 4), _ % 2 != 0) shouldEqual List(2, 3, 4)
    }
    "return Nil on empty list" in {
      dropWhile[Int](Nil, _ ⇒ true) shouldEqual Nil
    }
  }

  "dropWhile2" should {
    "drop the element until predicate get true" in {
      dropWhile2(List(1, 2, 3, 4))(_ % 2 != 0) shouldEqual List(2, 3, 4)  // 第二个参数组，不需要指明类型
    }
    "return Nil on empty list" in {
      dropWhile2(Nil)(_ ⇒ true) shouldEqual Nil
    }
  }

  "init" should {
    "throw exception on empty list" in {
      intercept[Exception] {
        init(Nil)
      }
    }
    "return all but the last element" in {
      init(List(1)) shouldEqual Nil
      init(List(1, 2, 3, 4)) shouldEqual List(1, 2, 3)
    }
  }

  "init2" should {
    "throw exception on empty list" in {
      intercept[Exception] {
        init2(Nil)
      }
    }
    "return all but the last element" in {
      init2(List(1)) shouldEqual Nil
      init2(List(1, 2, 3, 4)) shouldEqual List(1, 2, 3)
    }
  }

  "foldRight" should {
    "build sum with foldRight" in {
      def sum(l: List[Int]): Int = foldRight(0)(l)(_ + _)
      sum(List(1, 2, 3)) shouldEqual 6
      sum(Nil) shouldEqual 0
    }
    "build product with foldRight" in {
      def product(l: List[Double]): Double = foldRight(1.0)(l)(_ * _)
      product(List(1, 2, 3, 4)) shouldEqual 24.0
      product(Nil) shouldEqual 1.0
    }
  }

  "length4s" should {
    "return 0 on empty list" in {
      length4s(Nil) shouldEqual 0
    }
    "return the length of the given list" in {
      length4s(List("A", "B", "C")) shouldEqual 3
    }
  }

  "foldLeft" should {
    "build sum with foldLeft" in {
      sum2(List(1, 2, 3)) shouldEqual 6
      sum2(Nil) shouldEqual 0
    }
    "build product with foldLeft" in {
      product2(List(1, 2, 3, 4)) shouldEqual 24.0
      product2(Nil) shouldEqual 1.0
    }
    "build length with foldLeft" in {
      length2(Nil) shouldEqual 0
      length2(List("A", "B", "C")) shouldEqual 3
    }
  }

  "reverse" should {
    "return Nil on empty list" in {
      reverse(Nil) shouldEqual Nil
    }
    "work fine with non empty list" in {
      reverse(List("A", "B")) shouldEqual List("B", "A")
    }
  }

  "foldLeftViaFoldRight" should {
    "behave exactly like foldLeft" in {
      val xs = List(1, 2, 3, 4)
      foldLeftViaFoldRight(xs, 0)(_ + _) shouldEqual foldLeft(xs, 0)(_ + _)
      foldLeftViaFoldRight(xs, Nil: List[Int])((l, x) ⇒ Cons(x, l)) shouldEqual foldLeft(xs, List[Int]())((l, x) ⇒ Cons(x, l))
    }
  }

  "appendViaFoldRight" should {
    "work fine with empty list" in {
      appendViaFoldRight(Nil, Nil) shouldEqual Nil
      appendViaFoldRight(Nil, List(1, 2)) shouldEqual List(1, 2)
      appendViaFoldRight(List(1, 2), Nil) shouldEqual List(1, 2)
    }
    "work fine with non empty list" in {
      appendViaFoldRight(List(1, 2), List(3, 4)) shouldEqual List(1, 2, 3, 4)
    }
  }

  "appendViaFoldLeft" should {
    "work fine with empty list" in {
      appendViaFoldLeft(Nil, Nil) shouldEqual Nil
      appendViaFoldLeft(Nil, List(1, 2)) shouldEqual List(1, 2)
      appendViaFoldLeft(List(1, 2), Nil) shouldEqual List(1, 2)
    }
    "work fine with non empty list" in {
      appendViaFoldLeft(List(1, 2), List(3, 4)) shouldEqual List(1, 2, 3, 4)
    }
  }

  "concat" should {
    "return Nil on empty list" in {
      concat(Nil) shouldEqual Nil
      concat(List(Nil)) shouldEqual Nil
    }
    "flatten the give list" in {
      concat(List(List(1, 2), List("A", "B"), List('a', 'b'))) shouldEqual List(1, 2, "A", "B", 'a', 'b')
    }
  }

  "add1" should {
    "return Nil on empty list" in {
      add1(Nil) shouldEqual Nil
    }
    "add 1 to every element in the given list" in {
      add1(List(1, 2, 3)) shouldEqual List(2, 3, 4)
    }
  }

  "doubleToString" should {
    "return Nil on empty list" in {
      doubleToString(Nil) shouldEqual Nil
    }
    "convert every element to String" in {
      doubleToString(List(1.1, 2.2)) shouldEqual List("1.1", "2.2")
    }
  }

  "filter" should {
    "return Nil on empty list" in {
      filter(Nil: List[Int])(_ % 2 == 0) shouldEqual Nil
    }
    "return a list containing all elements that confirm to f" in {
      filter(List(1, 2, 3, 4))(_ % 2 == 0) shouldEqual List(2, 4)
    }
  }

  "filter2" should {
    "return Nil on empty list" in {
      filter2(Nil: List[Int])(_ % 2 == 0) shouldEqual Nil
    }
    "return a list containing all elements that confirm to f" in {
      filter2(List(1, 2, 3, 4))(_ % 2 == 0) shouldEqual List(2, 4)
    }
  }

  "filter3" should {
    "return Nil on empty list" in {
      filter3(Nil: List[Int])(_ % 2 == 0) shouldEqual Nil
    }
    "return a list containing all elements that confirm to f" in {
      filter3(List(1, 2, 3, 4))(_ % 2 == 0) shouldEqual List(2, 4)
    }
  }

  "flatMapViaFoldRight" should {
    "return Nil on empty list" in {
      flatMapViaFoldRight(Nil: List[Int])(x ⇒ List(x, x)) shouldEqual Nil
    }
    "work fine with non empty list" in {
      flatMapViaFoldRight(List(1, 2, 3))(x ⇒ List(x, x)) shouldEqual List(1, 1, 2, 2, 3, 3)
    }
  }

  "flatMapViaConcat" should {
    "return Nil on empty list" in {
      flatMapViaConcat(Nil: List[Int])(x ⇒ List(x, x)) shouldEqual Nil
    }
    "work fine with non empty list" in {
      flatMapViaConcat(List(1, 2, 3))(x ⇒ List(x, x)) shouldEqual List(1, 1, 2, 2, 3, 3)
    }
  }

  "filterViaFlatMap" should {
    "return Nil on empty list" in {
      filterViaFlatMap(Nil: List[Int])(_ % 2 == 0) shouldEqual Nil
    }
    "return a list containing all elements that confirm to f" in {
      filterViaFlatMap(List(1, 2, 3, 4))(_ % 2 == 0) shouldEqual List(2, 4)
    }
  }

  "addPairwise" should {
    "work fine with lists of same length" in {
      addPairwise(List(1, 2, 3), List(4, 5, 6)) shouldEqual List(5, 7, 9)
    }
    "work fine with lists of different length" in {
      addPairwise(List(1, 2, 3), List(4, 5, 6, 7)) shouldEqual List(5, 7, 9)
    }
  }

  "zipWith" should {
    "work fine with lists of same length" in {
      zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) shouldEqual List(5, 7, 9)
    }
    "work fine with lists of different length" in {
      zipWith(List(1, 2, 3), List("A", "B", "C", "D"))(_ + _) shouldEqual List("1A", "2B", "3C")
    }
  }

  "hasSubsequence" should {
    "return true if sub sequence is empty" in {
      hasSubsequence(Nil, Nil) shouldEqual true
    }
    "return true if xs has the sub sequence" in {
      hasSubsequence(List(1, 2, 3, 4), List(1, 2, 3)) shouldEqual true
      hasSubsequence(List(1, 2, 3, 4), List(1, 2, 3, 4)) shouldEqual true
    }
    "return false if xs does not have the sub sequence" in {
      hasSubsequence(List(1, 2, 3, 4), List(1, 4, 3)) shouldEqual false
      hasSubsequence(List(1, 2, 3, 4), List(1, 2, 3, 4, 5)) shouldEqual false
    }
  }

}
