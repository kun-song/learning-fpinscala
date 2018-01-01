package com.satansk.fpinscala.parsing

import com.satansk.fpinscala.testing.{Gen, Prop}

import scala.util.matching.Regex

/**
  * Author:  satansk
  * Email:   satansk@hotmail.com
  * Date:    17/12/31
  */

trait Parsers[ParseError, Parser[+_]] { self ⇒

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] = string(c.toString).map[String, Char](_.charAt(0))

  /**
    * or 只有当 p1 执行失败后，才会尝试执行 p2，因此 p2 应该是惰性求值（否则，可能出现死循环，程序无法停止）
    */
  def or[A](p1: Parser[A], p2: ⇒ Parser[A]): Parser[A]

  def succeed[A](a: A): Parser[A] = string("") map (_ ⇒ a)

  def slice[A](p: Parser[A]): Parser[String]

  /**
    * 语义：限制性 p1，然后执行 p2
    *
    * p1 执行失败时，不再执行 p2，所以 p2 应该是惰性求值
    */
  def product_1[A, B](p1: Parser[A], p2: ⇒ Parser[B]): Parser[(A, B)]

  /**
    * Exercise 9.1 使用 product 实现 map2，然后用 map2 和 many 实现 many1
    */
  def map2_1[A, B, C](p1: Parser[A], p2: ⇒ Parser[B])(f: (A, B) ⇒ C): Parser[C] =
    map(product(p1, p2))(x ⇒ f(x._1, x._2))

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  /**
    * Exercise 9.3 使用 or、map2 和 succeed 实现 many
    *
    * or:      (Parser[A], Parser[A]) => Parser[A]
    * map2:    (Parser[A], Parser[B]) => Parser[C]
    * succeed: A => Parser[A]
    *
    * 1. 当 p 运行成功时，将递归调用 many
    * 2. 当 p 运行失败时，将返回 succeed（or 的第二个参数也就是截止条件）
    *
    * map2 第二个参数递归调用 many，若其为严格求值，则 many 将永远无法停止，因为 p 运行失败时，也会继续调用 many，而我们的本意是，p 失败时，
    * 不再调用 many，而是执行 succeed；
    *
    * 所以 map2 的第二个参数必须是惰性求值，进而 product 第二个参数也必须是惰性求值（否则即使 map2 的第二个参数不立即求值，在 map2 实现内部，因为
    * 需要对 product 求值，product 的第二个参数引用了 map2 的第二个参数，从而也会导致其求值计算）。
    */
  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) or succeed(Nil)

  /**
    * Exercise 9.4 使用 map2 和 succeed 实现 listOfN
    *
    * 思路：与 many 类似，递归实现
    */
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(Nil)
    else map2(p, listOfN(n - 1, p))(_ :: _)

  /**
    * Exercise 9.5 若 map2 两个参数都是严格求值，则需要实现单独的组合子，专门用于惰性求值，尝试实现它
    */
  def wrap[A](p: ⇒ Parser[A]): Parser[A]

  def manyViaWrap[A](p: Parser[A]): Parser[List[A]] = map2(p, wrap(many(p)))(_ :: _) or succeed(Nil)

  /**
    * 若无 flatMap，则只能实现上下文无关文法的分析，上下文相关的无法实现，例如现在有 p1, p2，需要根据 p1 的值来进行 p2 的计算，
    * 如果没有 flatMap 则无法实现
    */
  def flatMap[A, B](p: Parser[A])(f: A ⇒ Parser[B]): Parser[B]

  /**
    * Exercise 9.6 使用 flatMap 以及其他组合子，实现上下文相关的分析器，实现 regex 函数
    */
  implicit def regex(r: Regex): Parser[String]

  /**
    * Exercise 9.7 使用 flatMap 实现 product 和 map2
    *
    * 原本 product 和 map2 是单独实现的，属于 primitive，但现在使用 flatMap 实现，所以 product map2 不再属于原语
    *
    * 两种方式：
    *
    * 1. flatMap + succeed
    * 2. flatMap + map
    */
  def product_2[A, B](p1: Parser[A], p2: ⇒ Parser[B]): Parser[(A, B)] =
    flatMap(p1)(a ⇒ flatMap(p2)(b ⇒ succeed(a, b)))

  def map2_2[A, B, C](p1: Parser[A], p2: ⇒ Parser[B])(f: (A, B) ⇒ C): Parser[C] =
    flatMap(p1)(a ⇒ flatMap(p2)(b ⇒ succeed(f(a, b))))

  def product[A, B](p1: Parser[A], p2: ⇒ Parser[B]): Parser[(A, B)] =
    flatMap(p1)(a ⇒ map(p2)((a, _)))

  def map2[A, B, C](p1: Parser[A], p2: ⇒ Parser[B])(f: (A, B) ⇒ C): Parser[C] =
    flatMap(p1)(a ⇒ map(p2)(f(a, _)))

  /**
    * Exercise 9.8 使用 flatMap 实现 map
    */
  def map[A, B](p: Parser[A])(f: A ⇒ B): Parser[B] =
    flatMap(p)(a ⇒ succeed(f(a)))

  /**
    * 隐式类型转换：String => Parser[String]
    */
  implicit def string(s: String): Parser[String]

  /**
    * 隐式类型转换：Parser[A] => ParserOps[A]
    */
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps(p)

  /**
    * 隐式类型转换：A => ParserOps[String]
    *
    * 该函数本身有隐式参数 A => Parser[String]，即任何可以转换为 Parser[String] 的类型 A，都可以自动转换为 ParserOps[String]
    */
  implicit def asStringParser[A](a: A)(implicit f: A ⇒ Parser[String]): ParserOps[String] = ParserOps(f(a))

  /**
    * 1. ParserOps 中也定义有 or 函数，此处使用 self 消除调用 or 的歧义
    */
  case class ParserOps[A](p: Parser[A]) {

    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def many[A]: Parser[List[A]] = self.many(p)

    def map[A, B](f: A ⇒ B): Parser[B] = self.map(p)(f)

    def slice[A]: Parser[String] = self.slice(p)

    /**
      * a ** b 和 a product b 都被代理到 product(a, b) 上
      */
    def product[A, B](p2: ⇒ Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def **[A, B](p2: ⇒ Parser[B]): Parser[(A, B)] = self.product(p, p2)

  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(input: Gen[String]): Prop =
      Prop.forAll(input) {
        s ⇒ run(p1)(s) == run(p2)(s)
      }

    /**
      * 法则 1：map(p)(x => x) == p
      */
    def mapLaw[A](p: Parser[A])(input: Gen[String]): Prop =
      equal(p, p.map(x ⇒ x))(input)
  }
}

object Parsers {


}
