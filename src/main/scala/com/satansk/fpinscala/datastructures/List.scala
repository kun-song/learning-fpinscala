package com.satansk.fpinscala.datastructures

import scala.annotation.tailrec

/**
  * Author:  satansk
  * Email:   satansk@hotmail.com
  * Date:    17/10/9
  */

/**
  * 1. 泛型 List
  * 2. + 协变符号，若 Dog 为 Animal 的子类，则 List[Dog] 是 List[Animal] 的子类
  */
sealed trait List[+A]

/**
  * 1. 空 List 构造器
  * 2. Nothing 是所有类型的子类，因为 List 为协变，所以 List[Nothing] 即 Nil，为所有 List 的子类
  */
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A] // 非空 List 构造器

// List 伴生对象，包含创建 List、操作 List 的函数
object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil          ⇒ 0
    case Cons(x, xs)  ⇒ x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          ⇒ 1.0
    case Cons(0.0, _) ⇒ 0.0
    case Cons(x, xs)  ⇒ x * product(xs)
  }

  /**
    * 1. 创建 List
    * 2. A* 为可变参数，接受 0 个或多个类型为 A 的参数
    * 3. as 类型为 Seq[A]
    */
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) ⇒ x
    case Nil                          ⇒ 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _))))  ⇒ x + y
    case Cons(h, t)                   ⇒ h + sum(t)
    case _                            ⇒ 101
  }

  /**
    * Exercise 3.2 implement `tail`
    *
    * 若元素在 => 右侧没有用到，最好将其写为 _
    */
  def tail[A](l: List[A]): List[A] = l match {
    case Nil        ⇒ throw new NoSuchMethodException("Nil.tail")
    case Cons(_, t) ⇒ t
  }

  /**
    * Exercise 3.3 implement `setHead`
    */
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil          ⇒ throw new NoSuchMethodException("Nil.setHead")
    case Cons(_, t)  ⇒ Cons(h, t)
  }

  /**
    * Exercise 3.4 implement `drop`
    *
    * drop 空表返回 Nil，这时因为使用 drop 时一般 List 长度未知，而 n 是其他地方算出来的，如果要抛出异常，则需要在调用 drop 之前
    * 先计算 List 长度，以保证 n 小于等于 List 长度，不符合实际
    */
  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (l, n) if n <= 0 ⇒ l
    case (Cons(_, t), n)  ⇒ drop(t, n - 1)
    case (Nil, _)         ⇒ Nil
  }

  /**
    * Exercise 3.5 implement `dropWhile`
    */
  def dropWhile[A](l: List[A], p: A ⇒ Boolean): List[A] = l match {
    case Cons(h, t) if p(h)   ⇒ dropWhile(t, p)
    case _                    ⇒ l
  }

  /**
    * 1. 上面 dropWhile 签名为：(l: List[A], p: A ⇒ Boolean)，虽然 l p 两次用到参数 A，使用时，需分别指明类型；
    * 2. 参数分组，类型信息从左到右传递，如下前面，Scala 可以推导出 p 中的 A 与 l 相同，调用时不需要指明类型信息；
    * 3. 将参数分组排序为多个参数列表，以最大化利用类型推导；
    */
  def dropWhile2[A](l: List[A])(p: A ⇒ Boolean): List[A] = l match {
    case Cons(h, t) if p(h) ⇒ dropWhile2(t)(p)
    case _                  ⇒ l
  }

  def append[A](xs: List[A], ys: List[A]): List[A] = xs match {
    case Nil        ⇒ ys
    case Cons(h, t) ⇒ Cons(h, append(t, ys))
  }

  /**
    * Exercise 3.6 implement `init`, return all but the last element of a list
    *
    * 1. 拷贝前 n - 1 个元素，效率低下
    * 2. 该解法符合递归直觉，但非尾递归，可能栈溢出
    */
  def init[A](l: List[A]): List[A] = l match {
    case Nil          ⇒ throw new Exception("Nil.init")
    case Cons(_, Nil) ⇒ Nil
    case Cons(h, t)   ⇒ Cons(h, init(t))
  }

  /**
    * 1. 对于 List，常见做法是在内部使用临时、可变缓存（lazy list 或 stream 不使用该方法）
    * 2. 外部不可见
    */
  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer

    val buf = new ListBuffer[A]
    def aux(cur: List[A]): List[A] = cur match {
      case Nil          ⇒ throw new Exception("Nil.init")
      case Cons(_, Nil) ⇒ List(buf.toList: _*)
      case Cons(h, t)   ⇒ buf += h; aux(t)
    }

    aux(l)
  }

  /**
    * foldRight
    *
    * 从结构上可理解为：用 z 替换 Nil，用 f 替换 Cons
    */
  def foldRight[A, B](z: B)(l: List[A])(f: (A, B) ⇒ B): B = l match {
    case Nil        ⇒ z
    case Cons(h, t) ⇒ f(h, foldRight(z)(t)(f))
  }

  /**
    * Exercise 3.7
    *
    * 不会提前终止，因为 foldRight 先遍历所有元素，将其展开，然后才真正执行 f；若要支持 early termination，需要
    * 使用非严格求值，在 Chapter 5 介绍。
    */

  /**
    * Exercise 3.8
    *
    * 结果为输入 list！因为 foldRight 可以认为是用 z 参数替换 Nil，用 f 替换 Cons，若 Nil 就是 z，f 就是 Cons，则结果与输入参数相同：
    *
    * foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_,_))
    * Cons(1, foldRight(List(2, 3), Nil:List[Int])(Cons(_,_)))
    * Cons(1, Cons(2, foldRight(List(3), Nil:List[Int])(Cons(_,_))))
    * Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil:List[Int])(Cons(_,_)))))
    * Cons(1, Cons(2, Cons(3, Nil)))
    */

  /**
    * Exercise 3.9 implement `length` by `foldRight`
    */
  def length4s[A](xs: List[A]): Int = foldRight(0)(xs)((_, y) ⇒ y + 1)

  /**
    * Exercise 3.10 foldRight 非尾递归，当 list 很大时可能发生栈溢出。请实现一个通用的、尾递归的 foldLeft 函数。
    */
  @tailrec
  def foldLeft[A, B](xs: List[A], z: B)(f: (B, A) ⇒ B): B = xs match {
    case Nil        ⇒ z
    case Cons(h, t) ⇒ foldLeft(t, f(z, h))(f)
  }

  /**
    * Exercise 3.11 用 foldLeft 实现 sum product length 函数
    */
  def sum2(xs: List[Int]): Int = foldLeft(xs, 0)(_ + _)
  def product2(xs: List[Double]): Double = foldLeft(xs, 1.0)(_ * _)
  def length2[A](xs: List[A]): Int = foldLeft(xs, 0)((x, _) ⇒ x + 1)  // foldRight 实现时：(_, y) => y + 1

  /**
    * Exercise 3.12 翻转列表
    */
  def reverse[A](xs: List[A]): List[A] = foldLeft(xs, Nil: List[A])((l, x) ⇒ Cons(x, l))


  /**
    * Exercise 3.13 implement `foldLeft` by `foldRight`
    *
    * 1. 使用 reverse + foldLeft 实现严格求值的 foldRight，以避免 foldRight 栈溢出，是一种常见技巧；
    */
  def foldRightViaFoldLeft[A, B](xs: List[A], z: B)(f: (A, B) ⇒ B): B = foldLeft(reverse(xs), z)((a, b) ⇒ f(b, a))

  def foldLeftViaFoldRight[A, B](xs: List[A], z: B)(f: (B, A) ⇒ B): B = foldRight(z)(reverse(xs))((a, b) ⇒ f(b, a))

  /**
    * Exercise 3.14 implement `append` via `foldLeft` or `foldRight`
    */
  def appendViaFoldLeft[A](xs: List[A], ys: List[A]): List[A] = foldLeft(reverse(xs), ys)((l, x) ⇒ Cons(x, l))

  def appendViaFoldRight[A](xs: List[A], ys: List[A]): List[A] = foldRight(ys)(xs)(Cons(_, _))

  /**
    * Exercise 3.15 List(List(...)) => List(...)
    *
    * foldLeft foldRight 都能实现，没看出有什么区别？
    */
  def concat[A](xss: List[List[A]]): List[A] = foldLeft(xss, Nil: List[A])(append)

  /**
    * Exercise 3.16 转换整数列表，对每个元素 +1
    */
  def add1(xs: List[Int]): List[Int] = foldRight(Nil: List[Int])(xs)((x, l) ⇒ Cons(x + 1, l))

  /**
    * Exercise 3.17 convert List[Double] to List[String]
    */
  def doubleToString(xs: List[Double]): List[String] = foldRight(Nil: List[String])(xs)((x, l) ⇒ Cons(x.toString, l))

  /**
    * Exercise 3.18 implement `map`
    *
    * 1. 符合直觉的解法是 foldRight，但目前实现的 foldRight 会导致栈溢出：map
    * 2. 用 foldRightViaFoldLeft 替代 foldRight，避免栈溢出：map2
    * 3. 使用内部缓存，外部不可见
    */
  def map[A, B](xs: List[A])(f: A => B): List[B] = foldRight(Nil: List[B])(xs)((x, l) ⇒ Cons(f(x), l))

  def map2[A, B](xs: List[A])(f: A ⇒ B): List[B] = foldRightViaFoldLeft(xs, Nil: List[B])((x, l) ⇒ Cons(f(x), l))

  def map3[A, B](xs: List[A])(f: A ⇒ B): List[B] = {
    var buffer = new scala.collection.mutable.ListBuffer[B]
    def aux(xs: List[A]): Unit = xs match {
      case Nil ⇒ ()
      case Cons(h, t) ⇒ buffer += f(h); aux(t)
    }
    aux(xs)

    List(buffer.toList: _*) // converting from the standard Scala list to the list we've defined here
  }

  /**
    * Exercise 3.19 implement `filter` 删除所有不满足 f 的元素
    *
    * 与 map 类似：
    * 1. 符合直觉的解法是 foldRight，但可能导致栈溢出
    * 2. 使用 foldRightViaFoldLeft 替换 foldRight，避免栈溢出
    * 3. 使用内部缓存，外部不可见
    */
  def filter[A](xs: List[A])(f: A ⇒ Boolean): List[A] = foldRight(Nil: List[A])(xs)((x, l) ⇒ if (f(x)) Cons(x, l) else l)

  def filter2[A](xs: List[A])(f: A ⇒ Boolean): List[A] = foldRightViaFoldLeft(xs, Nil: List[A])((x, l) ⇒ if(f(x)) Cons(x, l) else l)

  def filter3[A](xs: List[A])(f: A ⇒ Boolean): List[A] = {
    var buffer = new scala.collection.mutable.ListBuffer[A]
    def aux(xs: List[A]): Unit = xs match {
      case Nil                ⇒ ()
      case Cons(h, t) if f(h) ⇒ buffer += h; aux(t)
      case Cons(_, t)         ⇒ aux(t)
    }

    aux(xs)
    List(buffer.toList: _*)
  }

  /**
    * Exercise 3.20 implement `flatMap`
    *
    * 因为 f 返回 List，所以 map 会返回 List of List，但我们有时希望返回 List，所以需要 flatMap
    */
  def flatMapViaFoldRight[A, B](xs: List[A])(f: A ⇒ List[B]): List[B] = foldRight(Nil: List[B])(xs)((x, l) ⇒ append(f(x), l))

  def flatMapViaConcat[A, B](xs: List[A])(f: A ⇒ List[B]): List[B] = concat(map(xs)(f))

  /**
    * Exercise 3.21 implement `filter` via `flatMap`
    */
  def filterViaFlatMap[A](xs: List[A])(f: A ⇒ Boolean): List[A] = flatMapViaFoldRight(xs)(x ⇒ if (f(x)) List(x) else Nil)

  /**
    * Exercise 3.22 addPairwise(List(1, 2, 3)), List(4, 5, 6)) -> List(5, 7, 9)
    */
  def addPairwise(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
    case (Cons(h1, t1), Cons(h2, t2)) ⇒ Cons(h1 + h2, addPairwise(t1, t2))
    case _                            ⇒ Nil
  }

  /**
    * Exercise 3.23 implement `zipWith`
    */
  def zipWith[A, B, C](xs: List[A], ys: List[B])(f: (A, B) => C): List[C] = (xs, ys) match {
    case (Cons(h1, t1), Cons(h2, t2)) ⇒ Cons(f(h1, h2), zipWith(t1, t2)(f))
    case _                            ⇒ Nil
  }

  /**
    * Exercise 3.24 impelement `hasSubsequence`
    *
    * 1. Scala 中，任意两个值都可以用 == 比较是否相等
    */
  def hasSubsequence[A](xs: List[A], sub: List[A]): Boolean = (xs, sub) match {
    case (_, Nil)                                 ⇒ true
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 ⇒ startWith(t1, t2)
    case (Cons(h1, _), Cons(h2, t2)) if h1 != h2  ⇒ hasSubsequence(xs, t2)
  }

  // return true if xs start with ys
  def startWith[A](xs: List[A], ys: List[A]): Boolean = (xs, ys) match {
    case (_, Nil)                                 ⇒ true
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 ⇒ startWith(t1, t2)
    case _                                        ⇒ false
  }

}
