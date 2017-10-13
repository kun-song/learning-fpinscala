package com.satansk.fpinscala.datastructures

/**
  * Author:  satansk
  * Email:   satansk@hotmail.com
  * Date:    17/10/12
  */
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  /**
    * Exercise 3.25 统计 Tree 中节点数（ Leaf + Branch）
    */
  def sizet[A](t: Tree[A]): Int = t match {
    case Leaf(_)      ⇒ 1
    case Branch(l, r) ⇒ 1 + sizet(l) + sizet(r)
  }

  /**
    * Exercise 3.26 返回 Tree[Int] 中的最大元素
    */
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x)      ⇒ x
    case Branch(r, l) ⇒ maximum(r) max maximum(l)
  }

  /**
    * Exercise 3.27 `depth` 函数，从根节点到任何叶子节点的最大路径长度
    */
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_)      ⇒ 0
    case Branch(l, r) ⇒ (depth(l) max depth(r)) + 1
  }

  /**
    * Exercise 3.28 implement `map`
    */
  def map[A, B](t: Tree[A])(f: A ⇒ B): Tree[B] = t match {
    case Leaf(x)  ⇒ Leaf(f(x))
    case Branch(l, r) ⇒ Branch(map(l)(f), map(r)(f))
  }

  /**
    * Exercise 3.29 提取 `size` `maximum` `depth` `map` 函数共性，实现 fold 函数
    *
    * 1. 与 List 的 fold 类似，任何需要用 pattern matching 实现的函数都可以用 fold 实现
    */
  def fold[A, B](t: Tree[A])(f: A ⇒ B)(g: (B, B) ⇒ B): B = t match {
    case Leaf(x)      ⇒ f(x)
    case Branch(l, r) ⇒ g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int = fold(t)(_ ⇒ 1)(1 + _ + _)

  def maximumViaFold(t: Tree[Int]): Int = fold(t)(a ⇒ a)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int = fold(t)(_ ⇒ 0)((l, r) ⇒ 1 + (l max r))

  /**
    * 注意 a => Leaf(f(a)) 需要指定类型为 Tree[B]
    */
  def mapViaFold[A, B](t: Tree[A])(f: A ⇒ B): Tree[B] = fold(t)(v ⇒ Leaf(f(v)): Tree[B])(Branch(_, _))

  def mapViaFold2[A, B](t: Tree[A])(f: A ⇒ B): Tree[B] = {
    def leaf[A](v: A): Tree[A] = Leaf(v)  // leaf 显式返回 Tree[A]
    def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r) // branch 显式返回 Tree[A]

    fold(t)(v ⇒ leaf(f(v)))(branch)
  }

}