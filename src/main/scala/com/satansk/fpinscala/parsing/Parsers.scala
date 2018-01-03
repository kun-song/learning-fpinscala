package com.satansk.fpinscala.parsing

import java.util.regex.Pattern

import com.satansk.fpinscala.testing.{Gen, Prop}

import scala.util.matching.Regex

/**
  * Author:  satansk
  * Email:   satansk@hotmail.com
  * Date:    17/12/31
  */

trait Parsers[Parser[+_]] { self ⇒

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

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
    product(p1, p2) map (x ⇒ f(x._1, x._2))

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
    flatMap(p1)(a ⇒ p2 map ((a, _)))

  def map2[A, B, C](p1: Parser[A], p2: ⇒ Parser[B])(f: (A, B) ⇒ C): Parser[C] =
    flatMap(p1)(a ⇒ p2 map (f(a, _)))

  /**
    * Exercise 9.8 使用 flatMap 实现 map
    */
  def map[A, B](p: Parser[A])(f: A ⇒ B): Parser[B] =
    flatMap(p)(a ⇒ succeed(f(a)))

  /**
    * 单层错误提示信息，当 p 失败时，将 msg 作为错误消息
    */
  def label[A](msg: String)(p: Parser[A]): Parser[A]

  /**
    * 嵌套错误提示信息，当 p 失败时，将 msg 添加到由 p 返回的错误栈上
    */
  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  /**
    * 延迟提交到 p，知道它成功为止
    */
  def attempt[A](p: Parser[A]): Parser[A]

  /**
    * 顺序排列 p1, p2，并忽略 p2 的结果，因为不关心 p2 的结果，所以使用 slice 包裹
    */
  def skipR[A](p1: Parser[A], p2: Parser[Any]): Parser[A] =
    map2(p1, slice(p2))((a, _) ⇒ a)

  /**
    * 顺序排列 p1, p2，并忽略 p1 的结果，因为对 p1 结果并不关系，所以使用 slice 包裹
    */
  def skipL[B](p1: Parser[Any], p2: Parser[B]): Parser[B] =
    map2(slice(p1), p2)((_, b) ⇒ b)

  /**
    * 匹配 0 或多个空白字符的 Parser
    */
  def whitespace: Parser[String] = "\\s*".r

  /**
    * 匹配 1 个或多个数字的 Parser
    */
  def digits: Parser[String] = "\\d+".r

  /**
    * 尝试执行 p，并忽略结尾的空白
    */
  def token[A](p: Parser[A]): Parser[A] =
    attempt(p) <* whitespace

  /**
    * 将 p 包裹在 start 和 stop 之间
    */
  def surround[A](start: Parser[Any], stop: Parser[Any])(p: ⇒ Parser[A]): Parser[A] =
    start *> p <* stop

  /**
    * 1 个或多个重复的 p，使用 p2 分隔，并忽略 p2 的结果
    */
  def sep1[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    map2(p, many(p2 *> p))(_ :: _)

  /**
    * 0 或多个重复的 p，使用 p2 分隔，并忽略 p2 的结果
    */
  def sep[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    sep1(p, p2) or succeed(Nil)

  /**
    * 无脑消耗输入，直到遇到给定字符串的 Parser
    */
  def thru(s: String): Parser[String] = (".*?" + Pattern.quote(s)).r

  /**
    * 非转义字符 Parser
    */
  def quoted: Parser[String] = (string("\"") *> thru("\"")).map(_.dropRight(1))

  /**
    * 转义 + 非转义字符 Parser
    */
  def escapedQuoted: Parser[String] =
    token(quoted label "string literal")

  /**
    * Java 风格的浮点数字面值，例如：.1, -1.0, 1e9, 1E-23, etc.
    */
  def doubleString: Parser[String] =
    token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)

  def double: Parser[Double] =
    doubleString map (_.toDouble) label "double literal"

  /**
    * A parser that succeeds when given empty input.
    */
  def eof: Parser[String] =
    regex("\\z".r).label("unexpected trailing characters")

  /**
    * The root of the grammar, expects no further input following `p`.
    */
  def root[A](p: Parser[A]): Parser[A] =
    p <* eof

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

    def many: Parser[List[A]] = self.many(p)

    def map[B](f: A ⇒ B): Parser[B] = self.map(p)(f)

    def slice: Parser[String] = self.slice(p)

    /**
      * a ** b 和 a product b 都被代理到 product(a, b) 上
      */
    def product[A, B](p2: ⇒ Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def **[A, B](p2: ⇒ Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def *>[B](p2: ⇒ Parser[B]): Parser[B] = self.skipL(p, p2)

    def <*(p2: ⇒ Parser[Any]): Parser[A] = self.skipR(p, p2)

    def label(msg: String): Parser[A] = self.label(msg)(p)

    def scope(msg: String): Parser[A] = self.scope(msg)(p)

    def token: Parser[A] = self.token(p)

    def sep1(sep: Parser[Any]): Parser[List[A]] = self.sep1(p, sep)

    def sep(sep: Parser[Any]): Parser[List[A]] = self.sep(p, sep)

    def as[B](b: B): Parser[B] = self.map(self.slice(p))(_ => b)
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

/**
  * @param stack 保存全部错误提示信息的栈
  */
case class ParseError(stack: List[(Location, String)])

/**
  * 根据完整输入、当前输入的偏移量计算行列值
  *
  * @param input  完整输入
  * @param offset 当前输入的偏移量
  */
case class Location(input: String, offset: Int = 0) {
  lazy val line = input.slice(0, offset + 1).count(_ == "\n") + 1
  lazy val col = input.slice(0, offset + 1).lastIndexOf("\n") match {
    case -1         ⇒ offset + 1
    case lineStart  ⇒ offset - lineStart
  }
}

object Parsers {


}
