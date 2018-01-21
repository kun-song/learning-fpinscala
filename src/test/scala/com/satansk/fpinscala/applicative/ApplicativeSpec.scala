package com.satansk.fpinscala.applicative

import java.text.SimpleDateFormat
import java.util.Date

import org.scalatest.{Matchers, WordSpec}

/**
  * Author:  satansk
  * Email:   satansk@hotmail.com
  * Date:    18/1/21
  */
class ApplicativeSpec extends WordSpec with Matchers {

  import com.satansk.fpinscala.applicative.Applicative._

  "validForm" should {
    "catch all errors of three forms" in {
      validForm("", "19909-19", "123456789") shouldEqual
        Failure("Name can't be empty", Vector("Birthdate must be in the form yyyy-MM-dd", "Phone number must be 10 digits"))
    }

    "return failure if there is any error" in {
      validForm("", "1990-9-19", "1234567890") shouldEqual
        Failure("Name can't be empty")

      validForm("Kyle", "19909-19", "1234567890") shouldEqual
        Failure("Birthdate must be in the form yyyy-MM-dd")

      validForm("Kyle", "1990-9-19", "123456789") shouldEqual
        Failure("Phone number must be 10 digits")

      validForm("", "19909-19", "1234567890") shouldEqual
        Failure("Name can't be empty", Vector("Birthdate must be in the form yyyy-MM-dd"))
    }

    "return success if there is no errors" in {
      validForm("Kyle", "1990-9-19", "1234567890") shouldEqual
        Success(WebForm("Kyle", new SimpleDateFormat("yyyy-MM-dd").parse("1990-9-19"), "1234567890"))
    }
  }

}
