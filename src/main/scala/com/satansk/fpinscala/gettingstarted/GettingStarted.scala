package com.satansk.fpinscala.gettingstarted

import scala.annotation.tailrec

/**
  * Author:  satansk
  * Email:   satansk@hotmail.com
  * Date:    17/10/8
  */

/**
  * Scala 中，一个 Object 即为一个模块
  */
object MyModule {
  def abs(n: Int): Int = if (n < 0) -n else n

  def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  /**
    * 阶乘：尾递归实现
    *
    * 1. tailrec 注解，若函数不是尾递归，则编译报错
    * 2. 可能出现 Int 溢出
    */
  def factorial(n: Int): Int = {
    @tailrec
    def aux(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else aux(n - 1, acc * n)

    aux(n, 1)
  }

  def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d"
    msg.format(n, factorial(n))
  }

  /**
    * 阶乘：迭代实现
    */
  def factorial2(n: Int): Int = {
    var acc = 1
    var i = n
    while (i > 0) {
      acc = acc * i
      i = i - 1
    }
    acc
  }

  /**
    * Exercise 1: Write a function to compute the nth fibonacci number
    */
  def fib(n: Int): Int = {
    def aux(n: Int, pre: Int, cur: Int): Int =
      if (n <= 0) pre
      else aux(n - 1, cur, pre + cur)

    aux(n, 0, 1)
  }

  /**
    * formatAbs 与 formatFactorial 非常类似，可以抽象为一个函数
    */
  def formatResult(name: String, n: Int, f: Int ⇒ Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }
}

/**
  * 单态函数，只能用于 Double 数组
  */
object MonomorphicBinarySearch {
  def binarySearch(ds: Array[Double], key: Double): Int = {
    @tailrec
    def go(low: Int, mid: Int, high: Int): Int = {
      if (low > high) -1
      else {
        val mid2 = (low + high) / 2
        val d = ds(mid2)  // 使用函数调用方式 ds(index) 访问数组，而非 ds[index] 方式

        if (d == key) mid2
        else if (d > key) go(low, mid2, mid2 - 1)
        else go(mid2 + 1, mid2, high)
      }
    }

    go(0, 0, ds.length - 1)
  }
}

/**
  * 多态函数
  */
object PolymorphicFunctions {
  /**
    * Exercise 2: Implement a polymorphic function to check whether an `Array[A]` is sorted
    */
  def isSorted[A](as: Array[A], gt: (A, A) ⇒ Boolean): Boolean = {
    @tailrec
    def aux(cur: Int, next: Int): Boolean = {
      if (next >= as.length) true
      else {
        if (gt(as(next), as(cur))) aux(next, next + 1)
        else false
      }
    }

    aux(0, 1)
  }

  /**
    * 1. 多态函数，由于类型的操作未知（具体类型，如 String 等有很多预知操作），只能通过参数中的函数参数进行操作（或根据函数参数定义的其他操作）
    * 2. 常常能编译通过，且符合类型签名的实现方式，只有一种
    */
  def partial1[A, B, C](a: A, f: (A, B) ⇒ C): B ⇒ C = (b: B) ⇒ f(a, b)

  /**
    * Exercise 3: Implement `curry`
    *
    * 1. => 右结合，所以 A ⇒ B ⇒ C 与 A ⇒ (B ⇒ C) 等价
    * 2. Function2 特质已实现 curried
    */
  def curry[A, B, C](f: (A, B) ⇒ C): A ⇒ B ⇒ C = (a: A) ⇒ (b: B) ⇒ f(a, b)

  /**
    * Exercise 4: Implement `uncurry`
    *
    * 1. Function 对象已实现 uncurried
    */
  def uncurry[A, B, C](f: A ⇒ B ⇒ C): (A, B) ⇒ C = (a, b) ⇒ f(a)(b)

  /**
    * Exercise 5: Implement `compose`
    */
  def compose[A, B, C](f: B ⇒ C, g: A ⇒ B): A ⇒ C = a ⇒ f(g(a))
}