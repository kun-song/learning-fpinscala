package com.satansk.fpinscala.laziness

import org.scalatest.{Matchers, WordSpec}
import Stream._

/**
  * Author:  satansk
  * Email:   satansk@hotmail.com
  * Date:    17/10/15
  */
class StreamSpec extends WordSpec with Matchers {
  import com.satansk.fpinscala.laziness.Thunk._
  import com.satansk.fpinscala.laziness.Stream._

  "headOption" should {
    "return None on empty stream" in {
      Empty.headOption shouldEqual None
    }
    "return the first element in the stream" in {
      Stream(1, 2, 3).headOption shouldEqual Some(1)
    }
  }

  "toList" should {
    "turn the given Stream to List" in {
      Stream(1, 2, 3).toList shouldEqual List(1, 2, 3)
      Stream().toList shouldEqual Nil
    }
  }

  "toListBetter" should {
    "turn the given Stream to List" in {
      Stream(1, 2, 3).toListBetter shouldEqual List(1, 2, 3)
      Stream().toListBetter shouldEqual Nil
    }
  }

  "toListFast" should {
    "turn the given Stream to List" in {
      Stream(1, 2, 3).toListFast shouldEqual List(1, 2, 3)
      Stream().toListFast shouldEqual Nil
    }
  }

  "take" should {
    "return Empty on empty stream" in {
      Empty.take(1) shouldEqual Empty
    }
    "return Empty if n <= 0" in {
      Stream(1, 2, 3).take(0) shouldEqual Empty
      Stream(1, 2, 3).take(-1) shouldEqual Empty
    }
    "return all elements if n >= the list's length" in {
      Stream(1, 2, 3).take(2).toList shouldEqual List(1, 2)
      Stream(1, 2, 3).take(3).toList shouldEqual List(1, 2, 3)
      Stream(1, 2, 3).take(30).toList shouldEqual List(1, 2, 3)
    }
  }

  "drop" should {
    "return Empty on empty stream" in {
      Empty.drop(1) shouldEqual Empty
    }
    "return all elements if n <= 0" in {
      Stream(1, 2, 3).drop(0).toList shouldEqual List(1, 2, 3)
      Stream(1, 2, 3).drop(-1).toList shouldEqual List(1, 2, 3)
      Empty.drop(0) shouldEqual Empty
    }
    "return Empty if n >= the list's length" in {
      Stream(1, 2, 3).drop(3).toList shouldEqual Nil
      Stream(1, 2, 3).drop(30).toList shouldEqual Nil
    }
    "return all elements after n" in {
      Stream(1, 2, 3, 4).drop(2).toList shouldEqual List(3, 4)
    }
  }

  "takeWhile" should {
    "return Empty on empty stream" in {
      Empty.takeWhile(_ ⇒ true) shouldEqual Empty
    }
    "return Empty if the first element is not satisfied with p" in {
      Stream(1, 2, 3).takeWhile(_ % 2 == 0) shouldEqual Empty
    }
    "return the longest leading elements that satisfy p" in {
      Stream(1, 3, 5, 2, 4, 6, 7, 9).takeWhile(_ % 2 == 1).toList shouldEqual List(1, 3, 5)
    }
  }

  "exist" should {
    "return false on empty stream" in {
      Empty.exists(_ ⇒ true) shouldEqual false
    }
    "return true if the stream has at least one element satisfied f" in {
      Stream(1, 2, 3, 4).exists(_ == 3) shouldEqual true
    }
    "return true if the stream has no element satisfied f" in {
      Stream(1, 2, 3, 4).exists(_ > 30) shouldEqual false
    }
  }

  "existsViaFoldRight" should {
    "return false on empty stream" in {
      Empty.existsViaFoldRight(_ ⇒ true) shouldEqual false
    }
    "return true if the stream has at least one element satisfied f" in {
      Stream(1, 2, 3, 4).existsViaFoldRight(_ == 3) shouldEqual true
    }
    "return true if the stream has no element satisfied f" in {
      Stream(1, 2, 3, 4).existsViaFoldRight(_ > 30) shouldEqual false
    }
  }

  "foldRight" should {
    "work fine with empty stream" in {
      Empty.foldRight(Nil: List[Int])(_ :: _) shouldEqual Nil
    }
    "work fine with non empty stream" in {
      Stream(1, 2, 3).foldRight(Nil: List[Int])(_ + 1 :: _) shouldEqual List(2, 3, 4)
    }
  }

  "forAll" should {
    "return true on empty stream" in {
      Empty.forAll(_ ⇒ false) shouldEqual true
    }
    "return true if all elements satisfied with f" in {
      Stream(1, 2, 3, 4).forAll(_ > 0) shouldEqual true
    }
    "return false if at least one elements not satisfied with f" in {
      Stream(1, 2, 3, 4).forAll(_ > 1) shouldEqual false
    }
  }

  "takeWhileViaFoldRight" should {
    "return Empty on empty stream" in {
      Empty.takeWhileViaFoldRight(_ ⇒ true) shouldEqual Empty
    }
    "return Empty if the first element is not satisfied with p" in {
      Stream(1, 2, 3).takeWhileViaFoldRight(_ % 2 == 0) shouldEqual Empty
    }
    "return the longest leading elements that satisfy p" in {
      Stream(1, 3, 5, 2, 4, 6, 7, 9).takeWhileViaFoldRight(_ % 2 == 1).toList shouldEqual List(1, 3, 5)
    }
  }

  "headOptionViaFoldRight" should {
    "return None on empty stream" in {
      Empty.headOptionViaFoldRight shouldEqual None
    }
    "return the first element in the stream" in {
      Stream(1, 2, 3).headOptionViaFoldRight shouldEqual Some(1)
    }
  }

  "map" should {
    "return Empty on empty stream" in {
      Empty.map(_ ⇒ 1) shouldEqual Empty
    }
    "convert all element using f" in {
      Stream(1, 2, 3).map(_ + 1).toList shouldEqual List(2, 3, 4)
      Stream(Stream(2, 3), Empty).map(_.toList).toList shouldEqual List(List(2, 3), Nil)
    }
  }

  "filter" should {
    "return Empty on empty stream" in {
      Empty.filter(_ ⇒ true) shouldEqual Empty
    }
    "return all elements satisfied with the given predicate" in {
      Stream(1, 2, 3, 4).filter(_ % 2 == 0).toList shouldEqual List(2, 4)
    }
  }

  "append" should {
    "work fine with empty stream" in {
      Empty.append(Stream(1, 2, 3)).toList shouldEqual List(1, 2, 3)
      Stream(1, 2, 3).append(Empty).toList shouldEqual List(1, 2, 3)
      Empty.append(Empty) shouldEqual Empty
    }
    "return all elements of the given two stream" in {
      Stream(1, 2, 3).append(Stream("A", "B", "C")).toList shouldEqual List(1, 2, 3, "A", "B", "C")
    }
  }

  "flatMap" should {
    val f = (x: Int) ⇒ Stream(x, x * 2, x * 3)

    "return Empty on empty stream" in {
      Empty.flatMap(f) shouldEqual Empty
    }
    "work fine with non empty stream" in {
      Stream(1, 2, 3).flatMap(f).toList shouldEqual List(1, 2, 3, 2, 4, 6, 3, 6, 9)
    }
  }

  "ones" should {
    "contains infinite 1s" in {
      ones.take(3).toList shouldEqual List(1, 1, 1)
    }
  }

  "constants" should {
    "contains infinite n" in {
      constants(3).take(3).toList shouldEqual List(3, 3, 3)
    }
  }

  "from" should {
    "form an infinite stream from n" in {
      from(1).take(3).toList shouldEqual List(1, 2, 3)
    }
  }

  "fib" should {
    "return all fib numbers" in {
      fib.take(7).toList shouldEqual List(0, 1, 1, 2, 3, 5, 8)
    }
  }

  "unfold" should {
    "be able to generate empty stream" in {
      unfold(1)(_ ⇒ None) shouldEqual Empty
    }
    "be able to generate non empty stream" in {
      unfold(1)(x ⇒ Some(x * x, x + 1)).take(5).toList shouldEqual List(1, 4, 9, 16, 25)
    }
  }

  "onesViaUnfold" should {
    "contains infinite 1s" in {
      onesViaUnfold.take(3).toList shouldEqual List(1, 1, 1)
    }
  }

  "constantsViaUnfold" should {
    "contains infinite n" in {
      constantsViaUnfold(3).take(3).toList shouldEqual List(3, 3, 3)
    }
  }

  "fromViaUnfold" should {
    "form an infinite stream from n" in {
      fromViaUnfold(1).take(3).toList shouldEqual List(1, 2, 3)
    }
  }

  "fibViaUnfold" should {
    "return all fib numbers" in {
      fibViaUnfold.take(7).toList shouldEqual List(0, 1, 1, 2, 3, 5, 8)
    }
  }

  "mapViaUnfold" should {
    "return Empty on empty stream" in {
      Empty.mapViaUnfold(_ ⇒ 1) shouldEqual Empty
    }
    "convert all element using f" in {
      Stream(1, 2, 3).mapViaUnfold(_ + 1).toList shouldEqual List(2, 3, 4)
    }
  }

  "takeViaUnfold" should {
    "return Empty on empty stream" in {
      Empty.takeViaUnfold(1) shouldEqual Empty
    }
    "return Empty if n <= 0" in {
      Stream(1, 2, 3).takeViaUnfold(0) shouldEqual Empty
      Stream(1, 2, 3).takeViaUnfold(-1) shouldEqual Empty
    }
    "return all elements if n >= the list's length" in {
      Stream(1, 2, 3).takeViaUnfold(2).toList shouldEqual List(1, 2)
      Stream(1, 2, 3).takeViaUnfold(3).toList shouldEqual List(1, 2, 3)
      Stream(1, 2, 3).takeViaUnfold(30).toList shouldEqual List(1, 2, 3)
    }
  }

  "takeWhileViaUnfold" should {
    "return Empty on empty stream" in {
      Empty.takeWhileViaUnfold(_ ⇒ true) shouldEqual Empty
    }
    "return Empty if the first element is not satisfied with p" in {
      Stream(1, 2, 3).takeWhileViaUnfold(_ % 2 == 0) shouldEqual Empty
    }
    "return the longest leading elements that satisfy p" in {
      Stream(1, 3, 5, 2, 4, 6, 7, 9).takeWhileViaUnfold(_ % 2 == 1).toList shouldEqual List(1, 3, 5)
    }
  }

  "zipWith" should {
    "return Empty if either of its arguments is empty stream" in {
      Empty.zipWith(Stream(1, 2, 3))((_, x) ⇒ x) shouldEqual Empty
      Stream(1, 2, 3).zipWith(Empty)((x, _) ⇒ x) shouldEqual Empty
    }
    "zip the given two streams" in {
      Stream(1, 2, 3).zipWith(Stream("A", "B", "C"))(_ + _).toList shouldEqual List("1A", "2B", "3C")
      ones.zipWith(constants(2))(_ + _).take(3).toList shouldEqual List(3, 3, 3)
      ones.zipWith(Stream(1, 2, 3))(_ + _).toList shouldEqual List(2, 3, 4)
      Stream(1, 2, 3).zipWith(ones)(_ + _).toList shouldEqual List(2, 3, 4)
    }
  }

  "zip" should {
    "work fine with empty stream" in {
      Empty.zip(Stream(1, 2, 3)) shouldEqual Empty
      Stream(1, 2, 3).zip(Empty) shouldEqual Empty
    }
    "work fine with non empty stream" in {
      ones.zip(constants(2)).take(3).toList shouldEqual List((1, 2), (1, 2), (1, 2))
    }
  }

  "zipAll" should {
    "work fine with empty stream" in {
      Empty.zipAll(Stream(1, 2, 3)).take(3).toList shouldEqual List((None, Some(1)), (None, Some(2)), (None, Some(3)))
      Stream(1, 2, 3).zipAll(Empty).take(3).toList shouldEqual List((Some(1), None), (Some(2), None), (Some(3), None))
    }
    "work fine with non empty stream" in {
      ones.zipAll(constants(2)).take(2).toList shouldEqual List((Some(1), Some(2)), (Some(1), Some(2)))
      ones.zipAll(Empty).take(2).toList shouldEqual List((Some(1), None), (Some(1), None))
    }
  }

  "startWith" should {
    "work fine with empty stream" in {
      Empty.startWith(Empty) shouldEqual true
      Empty.startWith(Stream(1, 2, 3)) shouldEqual false
      Stream(1, 2, 3).startWith(Empty) shouldEqual true
    }
    "work fine with non empty stream" in {
      Stream(1, 2, 3, 4).startWith(Stream(1, 2, 3)) shouldEqual true
      Stream(1, 2, 3, 4).startWith(Stream(1, 3, 3)) shouldEqual false
      Stream(1, 2, 3, 4).startWith(Stream(2, 3)) shouldEqual false
    }
  }

  "tails" should {
    "return Empty on empty stream" in {
      Empty.tails.toList shouldEqual List(Empty)
    }
    "return all tail streams of the given stream" in {
      Stream(1, 2, 3).tails.map(_.toList).toList shouldEqual List(List(1, 2, 3), List(2, 3), List(3), Nil)
    }
  }

  "tails2" should {
    "return Empty on empty stream" in {
      Empty.tails2.toList shouldEqual List(Empty)
    }
    "return all tail streams of the given stream" in {
      Stream(1, 2, 3).tails2.map(_.toList).toList shouldEqual List(List(1, 2, 3), List(2, 3), List(3), Nil)
    }
  }

  "scanRight" should {
    "work fine with empty stream" in {
      (Empty: Stream[Int]).scanRight(0)(_ + _).toList shouldEqual List(0)
    }
    "work fine with non empty stream" in {
      Stream(1, 2, 3).scanRight(0)(_ + _).toList shouldEqual List(6, 5, 3, 0)
    }
  }

  "if2" should {
    "be able to use thunk to mock lazy evaluation" in {
      if2(true, () ⇒ { println("true"); 1 }, () ⇒ { println("false"); -1 })
    }
  }

  "if3" should {
    "be able to use thunk to mock lazy evaluation" in {
      if3(true, { println("true"); 1 }, { println("false"); -1 })
    }
  }

  "twice" should {
    "compute the parameter twice" in {
      twice(true, { println("twice"); 42 + 3 })
      twice(false, { println("once"); 42 + 3 })
    }
  }

  "once" should {
    "compute the parameter twice" in {
      once(true, { println("x"); 42 + 3 })
    }
  }
}
