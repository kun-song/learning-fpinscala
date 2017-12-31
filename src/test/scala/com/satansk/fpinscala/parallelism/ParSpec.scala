package com.satansk.fpinscala.parallelism

import java.util.concurrent.{ExecutorService, Executors}

import com.satansk.fpinscala.testing.{Gen, Prop}
import org.scalatest.{Matchers, WordSpec}

/**
  * Author:  satansk
  * Email:   satansk@hotmail.com
  * Date:    17/12/31
  */
class ParSpec extends WordSpec with Matchers {
  import com.satansk.fpinscala.parallelism.Par._

  val es: ExecutorService = Executors.newCachedThreadPool

  "map(unit(1))(_ + 1) == unit(2)" should {

    "prove 1" in {
      val mapProp = Prop.forAll(Gen.unit(Par.unit(1))) {
        i ⇒ map(i)(_ + 1)(es).get == unit(2)(es).get
      }

      Prop.run(mapProp)
    }

    "prove 2" in {
      val mapProp = Prop.check {
        map(unit(1))(_ + 1)(es).get == unit(2)(es).get
      }

      Prop.run(mapProp)
    }

    "prove 3" in {
      val mapProp = Prop.check {
        Prop.equal(map(unit(1))(_ + 1), unit(2))(es).get
      }

      Prop.run(mapProp)
    }

    "prove 4" in {
      val p = Prop.checkPar {
        Prop.equal(map(unit(1))(_ + 1), unit(2))
      }

      Prop.run(p)
    }

  }

  "map(y)(x => x) == y" should {
    "prove" in {
      val pint = Gen.choose(0, 10) map (Par.unit(_))

      val p = Prop.forAllPar(pint) {
        n ⇒ Prop.equal(Par.map(n)(x ⇒ x), n)
      }

      Prop.run(p)
    }
  }

  /**
    * Exercise 8.17 证明 fork(x) == x
    */
  "fork(x) == x" should {
    "prove" in {
      val pint = Gen.choose(0, 10) map (Par.unit(_))

      val p = Prop.forAllPar(pint) {
        n ⇒ Prop.equal(fork(n), n)
      }

      Prop.run(p)
    }
  }

}
