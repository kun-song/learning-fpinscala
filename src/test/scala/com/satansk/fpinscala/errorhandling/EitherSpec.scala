package com.satansk.fpinscala.errorhandling

import org.scalatest.{Matchers, WordSpec}

/**
  * Author:  satansk
  * Email:   satansk@hotmail.com
  * Date:    17/10/13
  */
class EitherSpec extends WordSpec with Matchers {
  import com.satansk.fpinscala.errorhandling.EitherT._

  "map" should {
    "work fine with Left and Right" in {
      Left("xxx") map (x ⇒ x) shouldEqual Left("xxx")
      Right(10) map (x ⇒ x * 3) shouldEqual Right(30)
    }
  }

  "flatMap" should {
    "work fine with Left and Right" in {
      Left("xxx") flatMap (x ⇒ x) shouldEqual Left("xxx")
      Right(10) flatMap (x ⇒ Right(x * 3)) shouldEqual Right(30)
    }
  }

  "orElse" should {
    "return the default Either on Left" in {
      Left("xxx") orElse Right(10) shouldEqual Right(10)
    }
    "return the original Either on Right" in {
      Right(10) orElse Right(-10) shouldEqual Right(10)
    }
  }

  "map2" should {
    "be able to convert parameters to Option" in {
      (Right("A") map2 Right(101)) (_ + _) shouldEqual Right("A101")
      (Right("A") map2 Left("xxx")) (_ + _) shouldEqual Left("xxx")
    }
  }

  "map2ViaFor" should {
    "be able to convert parameters to Option" in {
      (Right("A") map2ViaFor Right(101)) (_ + _) shouldEqual Right("A101")
      (Right("A") map2ViaFor Left("xxx")) (_ + _) shouldEqual Left("xxx")
    }
  }

  "sequence" should {
    "return Some(Nil) if the given Option list contains None" in {
      sequence(Left("xxx") :: Nil) shouldEqual Left("xxx")
      sequence(List(Right("A"), Right("B"), Left(101), Left(102))) shouldEqual Left(101)
    }
    "convert List[Option] to Option[List]" in {
      sequence(List(Right("A"), Right("B"), Right(101))) shouldEqual Right(List("A", "B", 101))
    }
  }

  "sequence2" should {
    "return Some(Nil) if the given Option list contains None" in {
      sequence2(Left("xxx") :: Nil) shouldEqual Left("xxx")
      sequence2(List(Right("A"), Right("B"), Left(101), Left(102))) shouldEqual Left(101)
    }
    "convert List[Option] to Option[List]" in {
      sequence2(List(Right("A"), Right("B"), Right(101))) shouldEqual Right(List("A", "B", 101))
    }
  }

  "traverse" should {
    "work fine with empty list" in {
      traverse(Nil)(Right(_)) shouldEqual Right(Nil)
    }
    "work fine on non empty list" in {
      traverse(List(1, 2, 3, 4))(Right(_)) shouldEqual Right(List(1, 2, 3, 4))
    }
    "return None if one element get None after applied f" in {
      traverse(List(1, 2, 3, 4))(x ⇒ if (x % 2 == 0) Right(x) else Left(x)) shouldEqual Left(1)
    }
  }

  "traverse2" should {
    "work fine with empty list" in {
      traverse2(Nil)(Right(_)) shouldEqual Right(Nil)
    }
    "work fine on non empty list" in {
      traverse2(List(1, 2, 3, 4))(Right(_)) shouldEqual Right(List(1, 2, 3, 4))
    }
    "return None if one element get None after applied f" in {
      traverse2(List(1, 2, 3, 4))(x ⇒ if (x % 2 == 0) Right(x) else Left(x)) shouldEqual Left(1)
    }
  }

}
