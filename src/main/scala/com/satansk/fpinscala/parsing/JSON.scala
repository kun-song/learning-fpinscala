package com.satansk.fpinscala.parsing

/**
  * Author:  satansk
  * Email:   satansk@hotmail.com
  * Date:    18/1/2
  */
trait JSON

/**
  * 1. JSON 的 key 只能是字符串
  * 2. JSON 的 value 可以是以下三种类型：
  *   （1）JSON 数组
  *   （2）JSON 对象
  *   （3）字面值：数字、字符串、布尔值
  */
object JSON {
  final case object JNull extends JSON
  final case class JNumber(get: Double) extends JSON
  final case class JString(get: String) extends JSON
  final case class JBool(get: Boolean) extends JSON
  final case class JArray(get: IndexedSeq[JSON]) extends JSON
  final case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JSON] = {
    /**
      * 隐藏 string => Parser 的隐式类型转换
      */
    import P.{string ⇒ _, _}

    /**
      * 默认将所有 String 使用 token 函数进行转换
      */
    implicit def tokenn[A](s: String) = token(P.string(s))

    def value: Parser[JSON] = array | obj

    def array = surround(P.string("["), P.string("]"))(
      value sep P.string(",") map (vs ⇒ JArray(vs.toIndexedSeq))) scope "array"

    def obj = surround(P.string("{"), P.string("}"))(
      keyVal sep P.string(",") map (kvs ⇒ JObject(kvs.toMap)) scope "object"
    )

    def keyVal = escapedQuoted ** (P.string(":") *> value)

    def lit = scope("literal") {
        P.string("null").as(JNull) |
          double.map(JNumber(_)) |
          escapedQuoted.map(JString(_)) |
          P.string("true").as(JBool(true)) |
          P.string("false").as(JBool(false))
    }

    root(whitespace *> (obj | array))

  }
}

