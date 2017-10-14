package com.satansk.fpinscala.errorhandling

import org.scalatest.{Matchers, WordSpec}

/**
  * Author:  satansk
  * Email:   satansk@hotmail.com
  * Date:    17/10/14
  */
class OptionSpec extends WordSpec with Matchers {
  import com.satansk.fpinscala.errorhandling.OptionT._

  "map" should {
    "work fine with None" in {
      NoneT map (_: Int ⇒ _) shouldEqual NoneT
    }
    "work fine with Some" in {
      SomeT(10) map (_ * 2) shouldEqual SomeT(20)
    }
  }

  "flatMap" should {
    val f = (x: Int) ⇒ if (x > 0) SomeT(x) else NoneT

    "work fine with None" in {
      NoneT flatMap f shouldEqual NoneT
    }
    "work fine with Some" in {
      SomeT(10) flatMap f shouldEqual SomeT(10)
      SomeT(-10) flatMap f shouldEqual NoneT
    }
  }

  "flatMap2" should {
    val f = (x: Int) ⇒ if (x > 0) SomeT(x) else NoneT

    "work fine with None" in {
      NoneT flatMap2 f shouldEqual NoneT
    }
    "work fine with Some" in {
      SomeT(10) flatMap2 f shouldEqual SomeT(10)
      SomeT(-10) flatMap2 f shouldEqual NoneT
    }
  }

  "getOrElse" should {
    "work fine with None" in {
      NoneT getOrElse 10 shouldEqual 10
    }
    "work fine with Some" in {
      SomeT(10) getOrElse -10 shouldEqual 10
    }
  }

  "orElse" should {
    "work fine with None" in {
      NoneT orElse SomeT(10) shouldEqual SomeT(10)
    }
    "work fine with Some" in {
      SomeT(10) orElse SomeT(-10) shouldEqual SomeT(10)
    }
  }

  "filter" should {
    "work fine with None" in {
      NoneT filter (_ ⇒ true) shouldEqual NoneT
    }
    "work fine with Some" in {
      SomeT(10) filter (_ % 2 == 0) shouldEqual SomeT(10)
      SomeT(10) filter (_ % 2 != 0) shouldEqual NoneT
    }
  }

  "variance" should {
    "return None on empty list" in {
      variance(Nil) shouldEqual NoneT
    }
    "return the variance on non empty list" in {
      variance(Seq(10, 10)) shouldEqual SomeT(0)
    }
  }

  "map2" should {
    "be able to convert parameters to Option" in {
      map2(SomeT("A"), SomeT(101))(_ + _) shouldEqual SomeT("A101")
    }
  }

  "sequence" should {
    "return Some(Nil) if the given Option list contains None" in {
      sequence(NoneT :: Nil) shouldEqual NoneT
      sequence(List(SomeT("A"), SomeT("B"), SomeT(101), NoneT)) shouldEqual NoneT
    }
    "convert List[Option] to Option[List]" in {
      sequence(List(SomeT("A"), SomeT("B"), SomeT(101))) shouldEqual SomeT(List("A", "B", 101))
    }
  }

  "sequence2" should {
    "return Some(Nil) if the given Option list contains None" in {
      sequence2(NoneT :: Nil) shouldEqual NoneT
      sequence2(List(SomeT("A"), SomeT("B"), SomeT(101), NoneT)) shouldEqual NoneT
    }
    "convert List[Option] to Option[List]" in {
      sequence2(List(SomeT("A"), SomeT("B"), SomeT(101))) shouldEqual SomeT(List("A", "B", 101))
    }
  }

  "traverse" should {
    "work fine with empty list" in {
      traverse(Nil)(SomeT(_)) shouldEqual SomeT(Nil)
    }
    "work fine on non empty list" in {
      traverse(List(1, 2, 3, 4))(SomeT(_)) shouldEqual SomeT(List(1, 2, 3, 4))
    }
    "return None if one element get None after applied f" in {
      traverse(List(1, 2, 3, 4))(x ⇒ if (x % 2 == 0) SomeT(x) else NoneT) shouldEqual NoneT
    }
  }

  "traverse2" should {
    "work fine with empty list" in {
      traverse2(Nil)(SomeT(_)) shouldEqual SomeT(Nil)
    }
    "work fine on non empty list" in {
      traverse2(List(1, 2, 3, 4))(SomeT(_)) shouldEqual SomeT(List(1, 2, 3, 4))
    }
    "return None if one element get None after applied f" in {
      traverse2(List(1, 2, 3, 4))(x ⇒ if (x % 2 == 0) SomeT(x) else NoneT) shouldEqual NoneT
    }
  }

  "sequenceViaTraverse" should {
    "return Some(Nil) if the given Option list contains None" in {
      sequenceViaTraverse(NoneT :: Nil) shouldEqual NoneT
      sequenceViaTraverse(List(SomeT("A"), SomeT("B"), SomeT(101), NoneT)) shouldEqual NoneT
    }
    "convert List[Option] to Option[List]" in {
      sequenceViaTraverse(List(SomeT("A"), SomeT("B"), SomeT(101))) shouldEqual SomeT(List("A", "B", 101))
    }
  }

}
