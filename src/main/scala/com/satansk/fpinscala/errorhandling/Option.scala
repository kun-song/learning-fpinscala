package com.satansk.fpinscala.errorhandling

/**
  * Author:  satansk
  * Email:   satansk@hotmail.com
  * Date:    17/10/14
  */
sealed trait OptionT[+A] {

  /**
    * Exercise 4.1 implement `map` `flatMap` `getOrElse` `orElse` `filter`
    */
  def map[B](f: A ⇒ B): OptionT[B] = this match {
    case NoneT     ⇒ NoneT
    case SomeT(v)  ⇒ SomeT(f(v))
  }

  // 函数组合
  def flatMap[B](f: A ⇒ OptionT[B]): OptionT[B] = this map f getOrElse NoneT

  // 显式 pattern matching
  def flatMap2[B](f: A ⇒ OptionT[B]): OptionT[B] = this match {
    case NoneT    ⇒ NoneT
    case SomeT(v) ⇒ f(v)
  }

  def getOrElse[B >: A](default: B): B = this match {
    case NoneT    ⇒ default
    case SomeT(v) ⇒ v
  }

  // 函数组合
  def orElse[B >: A](ob: ⇒ OptionT[B]): OptionT[B] = this map (SomeT(_)) getOrElse ob

  // 显式 pattern matching
  def orElse2[B >: A](ob: ⇒ OptionT[B]): OptionT[B] = this match {
    case NoneT    ⇒ ob
    case _        ⇒ this
  }

  def filter(f: A ⇒ Boolean): OptionT[A] = this match {
    case SomeT(v) if f(v) ⇒ this
    case _                ⇒ NoneT
  }

}

case object NoneT extends OptionT[Nothing]
case class SomeT[+A](get: A) extends OptionT[A]

object OptionT {

  def mean(xs: Seq[Double]): OptionT[Double] = xs match {
    case Nil  ⇒ NoneT
    case _    ⇒ SomeT(xs.sum / xs.length)
  }

  /**
    * Exercise 4.2 implement `variance` 方差函数
    *
    * 统计中的方差（样本方差）是每个样本值与全体样本值的平均数之差的平方值的平均数
    *
    * 1. 不需要显式处理空序列
    * 2. 熟悉函数组合方式
    */
  def variance(xs: Seq[Double]): OptionT[Double] =
    mean(xs) flatMap (m ⇒ mean(xs.map(x ⇒ math.pow(x - m, 2))))

  /**
    * Exercise 4.3 implement `map2`
    */
  def map2[A, B, C](a: OptionT[A], b: OptionT[B])(f: (A, B) ⇒ C): OptionT[C] =
    a flatMap (aa ⇒ b map (bb ⇒ f(aa, bb)))

  /**
    * Exercise 4.4 implement `sequence`
    */
  def sequence[A](xs: List[OptionT[A]]): OptionT[List[A]] =
    xs.foldRight(SomeT(Nil): OptionT[List[A]])((h, t) ⇒ map2(h, t)(_ :: _))

  // pattern matching solution
  def sequence2[A](xs: List[OptionT[A]]): OptionT[List[A]] = xs match {
    case Nil     ⇒ SomeT(Nil)
    case h :: t  ⇒ h flatMap (hh ⇒ sequence2(t) map (hh :: _))
  }

  def traverse[A, B](xs: List[A])(f: A ⇒ OptionT[B]): OptionT[List[B]] =
    xs.foldRight(SomeT(Nil): OptionT[List[B]])((h, t) ⇒ map2(f(h), t)(_ :: _))

  def traverse2[A, B](xs: List[A])(f: A ⇒ OptionT[B]): OptionT[List[B]] = xs match {
    case Nil    ⇒ SomeT(Nil)
    case h :: t ⇒ map2(f(h), traverse2(t)(f))(_ :: _)
  }

  /**
    * Exercise 4.5 implement `sequence` via `traverse`
    */
  def sequenceViaTraverse[A](xs: List[OptionT[A]]): OptionT[List[A]] = traverse(xs)(o ⇒ o)

  /**
    * Try 将基于异常的 API 转化为基于 Option 的 API（下面实现丢失了异常信息，仅返回 NoneT）。
    */
  def Try[A](a: ⇒ A): OptionT[A] =
    try SomeT(a)
    catch { case _: Exception ⇒ NoneT }

  /**
    * 将函数 A => B 提升为 Option[A] => Option[B]
    */
  def lift[A, B](f: A ⇒ B): OptionT[A] ⇒ OptionT[B] = _ map f

}

