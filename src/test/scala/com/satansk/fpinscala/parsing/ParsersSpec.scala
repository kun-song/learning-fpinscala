package com.satansk.fpinscala.parsing

import org.scalatest.{Matchers, WordSpec}

/**
  * Author:  satansk
  * Email:   satansk@hotmail.com
  * Date:    18/1/1
  */
class ParsersSpec extends WordSpec with Matchers {

  /**
    * Exercise 9.2 尝试总结 product 的法则
    *
    * 1. product 近似满足结合律，即 a ** (b ** c) 约等于 (a ** b) ** c；
    * 2. 唯一区别：pair 组合方式，一个是 (A, (B, C))，一个是 ((A, B), C)，将两者 flatten 后，就一致了；
    */
  "product" should {

    def unbiasL[A, B, C](p: ((A,B), C)): (A,B,C) = (p._1._1, p._1._2, p._2)
    def unbiasR[A,B,C](p: (A, (B,C))): (A,B,C) = (p._1, p._2._1, p._2._2)

    /**
      * a ** (b ** c) map (unbiasR) ~= (a ** b) ** c map (unbiasL)
      */
    "is associative" in {

    }
  }

}
