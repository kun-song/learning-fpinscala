package com.satansk.fpinscala.laziness

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

import Stream._

/**
  * Author:  satansk
  * Email:   satansk@hotmail.com
  * Date:    17/10/15
  */
sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty      ⇒ None
    case Cons(h, _) ⇒ Some(h()) // 对 h thunk 显式调用 h() 强制求值
  }

  /**
    * Exercise 5.1 将 Stream 转化为 List，会被强制求值，List 中包含所有元素的值
    *
    * 注意：因为不是尾递归，所以存在栈溢出风险
    */
  def toList: List[A] = this match {
    case Empty      ⇒ Nil
    case Cons(h, t) ⇒ h() :: t().toList // h t 都需要被强制求值
  }

  // 尾递归版本
  def toListBetter: List[A] = {
    @tailrec
    def aux(xs: Stream[A], acc: List[A]): List[A] = xs match {
      case Empty      ⇒ acc
      case Cons(h, t) ⇒ aux(t(), h() :: acc)
    }

    aux(this, Nil).reverse
  }

  // 使用内部缓存，避免尾递归版本中的 reverse
  def toListFast: List[A] = {
    var buffer = new ListBuffer[A]
    @tailrec
    def aux(xs: Stream[A]): List[A] = xs match {
      case Empty      ⇒ buffer.toList
      case Cons(h, t) ⇒ buffer += h(); aux(t())
    }

    aux(this)
  }

  /**
    * Exercise 5.2 实现 `take` `drop` 函数
    */
  def take(n: Int): Stream[A] = (this, n) match {
    case (Cons(h, t), n) if n > 0 ⇒ Cons(h, () ⇒ t() take (n - 1))
    case _                        ⇒ Empty
  }

  // tail recursive 函数必须是 private 或者 final
  @tailrec
  final def drop(n: Int): Stream[A] = (this, n) match {
    case (Cons(_, t), n) if n > 0  ⇒ t() drop (n - 1)
    case _                         ⇒ this
  }

  /**
    * Exercise 5.3 implement `takeWhile`
    */
  def takeWhile(p: A ⇒ Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) ⇒ Cons(h, () ⇒ t() takeWhile p)
    case _                    ⇒ Empty
  }

  /**
    * Stream 的 tail 是惰性的，所以当 f(h()) 为 true 时，函数返回，且剩余的 tail 部分不会被计算
    */
  def exists(f: A ⇒ Boolean): Boolean = this match {
    case Cons(h, t) ⇒ f(h()) || t().exists(f)
    case Empty      ⇒ false
  }

  /**
    * 注意 f 第二个参数为惰性求值，若函数 f 实际计算时，未引用第二个参数，则此时 tail 部分的递归不会计算
    */
  def foldRight[B](z: ⇒ B)(f: (A, ⇒ B) ⇒ B): B = this match {
    case Cons(h, t) ⇒ f(h(), t().foldRight(z)(f))
    case Empty      ⇒ z
  }

  def existsViaFoldRight(f: A ⇒ Boolean): Boolean = foldRight(false)((h, t) ⇒ f(h) || t)

  /**
    * Exercise 5.4 实现 `forAll` 函数，若有不匹配的元素，立刻终止
    */
  def forAll(f: A ⇒ Boolean): Boolean = foldRight(true)((h, t) ⇒ f(h) && t)

  /**
    * Exercise 5.5 使用 foldRight 实现 `takeWhile`
    */
  def takeWhileViaFoldRight(f: A ⇒ Boolean): Stream[A] =
    this.foldRight(Empty: Stream[A])((h, t) ⇒ if (f(h)) Cons(() ⇒ h, () ⇒ t) else Empty)

  /**
    * Exercise 5.6 使用 foldRight 实现 `headOption`
    */
  def headOptionViaFoldRight: Option[A] = this.foldRight(None: Option[A])((h, _) ⇒ Some(h))

  /**
    * Exercise 5.7 使用 foldRight 实现 `map` `filter` `append` `flatMap`，其中 `append` 为非严格求值
    */
  def map[B](f: A ⇒ B): Stream[B] = foldRight(Empty: Stream[B])((h, t) ⇒ Cons(() ⇒ f(h), () ⇒ t))

  def filter(f: A ⇒ Boolean): Stream[A] = foldRight(Empty: Stream[A])((h, t) ⇒ if (f(h)) Cons(() ⇒ h, () ⇒ t) else t)

  def append[B >: A](xs: ⇒ Stream[B]): Stream[B] = foldRight(xs)((h, t) ⇒ Cons(() ⇒ h, () ⇒ t))

  def flatMap[B](f: A ⇒ Stream[B]): Stream[B] = foldRight(Empty: Stream[B])((h, t) ⇒ f(h) append t)

  /**
    * 虽然 filter 转换了整个 stream，但转换是惰性的，所以 find 遇到匹配的第一个元素时，就会返回
    */
  def find(p: A ⇒ Boolean): Option[A] = filter(p).headOption

  /**
    * Exercise 5.13 使用 `unfold` 实现 `map` `take` `takeWhile` `zipWith` `zipAll` 函数
    */
  def mapViaUnfold[B](f: A ⇒ B): Stream[B] = unfold(this) {
    case Empty  ⇒ None
    case Cons(h, t) ⇒ Some(f(h()), t())
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, t), n) if n > 0  ⇒ Some(h(), (t(), n - 1))
    case _                         ⇒ None
  }

  def takeWhileViaUnfold(f: A ⇒ Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if f(h()) ⇒ Some(h(), t())
    case _                    ⇒ None
  }

  def zipWith[B, C](xs: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, xs)) {
    case (Cons(h1, t1), Cons(h2, t2)) ⇒ Some(f(h1(), h2()), (t1(), t2()))
    case _                            ⇒ None
  }

  // zipWith 特殊情况
  def zip[B](xs: Stream[B]): Stream[(A, B)] = zipWith(xs)((_, _))

  def zipWithAll[B, C](xs: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] = unfold((this, xs)) {
    case (Cons(h1, t1), Cons(h2, t2)) ⇒ Some(f(Some(h1()), Some(h2())), (t1(), t2()))
    case (Cons(h1, t1), Empty)        ⇒ Some(f(Some(h1()), None), (t1(), Empty))
    case (Empty, Cons(h2, t2))        ⇒ Some(f(None, Some(h2())), (Empty, t2()))
    case (Empty, Empty)               ⇒ None
  }

  def zipAll[B](xs: Stream[B]): Stream[(Option[A], Option[B])] = zipWithAll(xs)((_, _))

  def zipAll2[B](xs: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, xs)) {
    case (Cons(h1, t1), Cons(h2, t2)) ⇒ Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (Cons(h1, t1), Empty)        ⇒ Some((Some(h1()), None), (t1(), Empty))
    case (Empty, Cons(h2, t2))        ⇒ Some((None, Some(h2())), (Empty, t2()))
    case (Empty, Empty)               ⇒ Some((None, None), (Empty, Empty))
  }

  /**
    * Exercise 5.14 使用已实现的函数重新实现 startWith 函数，检查给定 Stream 是否是本 Stream 的前缀
    *
    * 1. Some("A") == Some("A") => true
    */
  def startWith[A](s: Stream[A]): Boolean = this.zipAll(s).takeWhile(!_._2.isEmpty) forAll {
    case (h1, h2) ⇒ h1 == h2
  }

  /**
    * Exercise 5.15 implement `tails` via `unfold`
    */
  def tails: Stream[Stream[A]] = unfold(this) {
    case Empty  ⇒ None
    case x      ⇒ Some((x, x drop 1)) // 该分支要在 Empty 之后，否则 x 将匹配所有！
  } append Stream(Empty)

  def tails2: Stream[Stream[A]] = unfold(this) {
    case Cons(h, t) ⇒ Some((Cons(h, t), t()))
    case _          ⇒ None
  } append Stream(Empty)

  def hasSubsequence[A](sub: Stream[A]): Boolean = tails exists (_ startWith sub)

  /**
    * Exercise 5.16 返回 `tails`，实现 `scanRight` 函数
    */
  def scanRight[B](z: B)(f: (A, ⇒ B) ⇒ B): Stream[B] =
    foldRight((z, Stream(z)))((a, b) ⇒ {
      lazy val b1 = b._1
      val zz = f(a, b1)
      (zz, cons(zz, b._2))
    })._2

}

case object Empty extends Stream[Nothing]
/**
  * case class 中不能使用 call by name 语法，要实现惰性计算，只能使用显式 thunk
  */
case class Cons[+A](h: () ⇒ A, t: () ⇒ Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: ⇒ A, tl: ⇒ Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() ⇒ head, () ⇒ tail)
  }

  def empty[A](): Stream[A] = Empty

  def apply[A](xs: A*): Stream[A] =
    if (xs.isEmpty) empty()
    else cons(xs.head, apply(xs.tail: _*))

  // 由 1 组成的无限流
  def ones: Stream[Int] = cons(1, ones)

  /**
    * Exercise 5.8 泛化 ones，实现 constants
    */
  def constants[A](x: A): Stream[A] = cons(x, constants(x))

  /**
    * Exercise 5.9 实现 `from` 函数，产生 n, n+1, n+2, ... 的无限流
    */
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  /**
    * Exercise 5.10 实现 `fib`，生成斐波那契数列
    */
  def fib: Stream[Int] = {
    def aux(pre: Int, cur: Int): Stream[Int] = cons(pre, aux(cur, pre + cur))
    aux(0, 1)
  }

  /**
    * Exercise 5.11 实现 `unfold`
    *
    * 1. 可以使用显式模式匹配实现
    * 2. 也可以用 fold map 函数组合方式实现
    */
  def unfold[A, S](z: S)(f: S ⇒ Option[(A, S)]): Stream[A] = f(z) match {
    case None         ⇒ Empty
    case Some((a, s)) ⇒ cons(a, unfold(s)(f))
  }

//  def unfoldViaFold[A, S](z: S)(f: S ⇒ Option[(A, S)]): Stream[A] = ???
//
//  def unfoldViaMap[A, S](z: S)(f: S ⇒ Option[(A, S)]): Stream[A] = ???

  /**
    * Exercise 5.12 使用 `unfold` 实现 `fibs` `from` `constants` `ones`
    */
  def onesViaUnfold: Stream[Int] = unfold(1)(_ ⇒ Some(1, 1))

  def constantsViaUnfold[A](n: A): Stream[A] = unfold(n)(n ⇒ Some(n, n))

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(n ⇒ Some(n, n + 1))

  def fibViaUnfold: Stream[Int] = unfold((0, 1)) { case (pre, cur) ⇒ Some(pre, (cur, pre + cur)) }

}

object Thunk {

  /**
    * Scala 函数参数默认是严格求值，要模拟非严格求值，需要使用 thunk：
    * 1. if2 接受 thunk 作为参数：() => A
    * 2. if2 内部，显式调用 thunk，获取结果：onTrue() & onFalse()
    * 3. 调用者需要显式创建 thunk 进行调用
    */
  def if2[A](cond: Boolean, onTrue: () ⇒ A, onFalse: () ⇒ A): A =
    if (cond) onTrue()
    else      onFalse()

  /**
    * 使用 thunk 模拟非严格求值非常常见，因此 Scala 提供了更简洁的语法支持：
    * 1. => 表明该参数是非严格求值
    * 2. 非严格求值函数，在每次引用时，都会求值一次：若多次引用，将重复计算，效率较低
    * 3. 调用时，自动将参数包装为 thunk，无需手动创建
    */
  def if3[A](cond: Boolean, onTrue: ⇒ A, onFalse: ⇒ A): A =
    if (cond) onTrue
    else      onFalse

  /**
    * 1. x 将会计算两次，因为 x + x 引用了两次 x，每次都会计算
    * 2. 使用 twice(true, { println("x"); 41 + 2 }) 会打印两次 x，说明参数被计算了两次
    * 3. => 不会自动缓存计算结果
    */
  def twice(cond: Boolean, x: ⇒ Int): Int =
    if (cond) x + x
    else x

  /**
    * 1. 使用 lazy val 修饰的变量，在定义时不会执行计算，在首次引用时才计算，且后续引用，使用缓存，不会触发重复计算
    * 2. t 在 t + t 中第一个引用时计算，且 + 后的第二个引用不会重复计算，而是使用之前缓存的结果
    */
  def once(cond: Boolean, x: ⇒ Int): Int = {
    lazy val t = x
    if (cond) t + t
    else t
  }

}