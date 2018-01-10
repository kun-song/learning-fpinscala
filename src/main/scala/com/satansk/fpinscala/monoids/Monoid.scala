package com.satansk.fpinscala.monoids

import com.satansk.fpinscala.datastructures.{Branch, Leaf, Tree}

import scala.language.higherKinds

/**
  * Author:  satansk
  * Email:   satansk@hotmail.com
  * Date:    18/1/4
  */

/**
  * monoid（幺半群），第一个纯代数结构。
  *
  * monoid 法则：结合律（associativity）、同一律（identity）法则被一起称为 monoid 法则
  *
  * 一个 monoid 由 3 部分组成：
  *
  * 1. 类型 A
  * 2. 二元操作 op
  *   （1）op 满足结合律，即 op(op(x, y), z) == op(x, op(y, z))
  * 3. 单位元 zero
  *   （1）zero 满足同一律，即 op(x, zero) == x 或 op(zero, x) == x
  *
  * 不同 monoid 实例之间，除了都满足 monoid 代数法则外，很少有其他关联。怎么理解 monoid 呢，monoid 就是一个类型 + 一些操作 + 一些法则，
  * 是纯抽象的概念，可以对应到具体情况，但具体情况无法代表 monoid，因为 monoid 是纯粹的抽象概念（白马非🐴）。
  */
trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  /**
    * Exercise 10.1 给出整数相加、整数相乘和布尔操作的 monoid 实例
    */
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    def zero: Boolean = true
  }

  /**
    * Exercise 10.2 给出能够组合 Option 值的 monoid 实例
    *
    * 注意：
    *
    * 1. optionMonoid 的 op 方法有两种实现，即 a1 orElse a2 和 a2 orElse a1，两种实现都满足 monoid 法则，但两者并不等价；
    * 2. 这实际是个通用问题，每个 monoid 的 op 操作都有个顺序相反的同类，它们通常并不相同，但又同时满足 monoid 法则，都是合法实现；
    * 3. 而 booleanOr 和 booleanAnd 与其顺序相反的实现完全等价，这是因为 || 和 && 操作，既满足结合律，又满足交换律（monoid 法则未要求满足交换律）；
    */
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    def zero: Option[A] = None
  }

  /**
    * 获取 m 的 dual Monoid
    */
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(a1: A, a2: A): A = m.op(a2, a1)
    def zero: A = m.zero
  }

  def firstOptionM[A]: Monoid[Option[A]] = optionMonoid[A]

  def lastOptionM[A]: Monoid[Option[A]] = dual(optionMonoid[A])

  /**
    * Exercise 10.3 参数、返回值类型相同的函数被称为自函数（endofunction），为 endofunction 编写一个 monoid
    *
    * 注意：两种函数组合方式都符合 monoid 法则，即 f compose g 和 f andThen g
    */
  def endoMonoid[A]: Monoid[A ⇒ A] = new Monoid[A ⇒ A] {
    def op(a1: A ⇒ A, a2: A ⇒ A): A ⇒ A = a1 compose a2
    def zero: A ⇒ A = a ⇒ a
  }

  /**
    * Exercise 10.4 为 monoid 法则实现一个属性，并使用该属性测试已经编写的 monoid
    */
  import com.satansk.fpinscala.testing._
  import Prop._

  def monoidLaws[A](m: Monoid[A], g: Gen[A]): Prop =
    // 结合律
    forAll(for {
      x ← g
      y ← g
      z ← g
    } yield (x, y, z)){
      xyz ⇒ m.op(m.op(xyz._1, xyz._2), xyz._3) == m.op(xyz._1, m.op(xyz._2, xyz._3))
    } &&
    // 同一律
    forAll(g) {
      x ⇒ m.op(x, m.zero) == x && m.op(m.zero, x) == x
    }

  /**
    * 使用 Monoid[A] 折叠列表 List[A]，比较通用，因为不需要知道类型 A 具体是什么。
    *
    * 局限：类型 A 必须存在 Monoid[A]
    */
  def concatenate[A](xs: List[A], m: Monoid[A]): A = xs.foldLeft(m.zero)(m.op)

  /**
    * Exercise 10.5 实现 foldMap 函数
    *
    * 突破 concatenate 函数的局限，即使 A 不存在 Monoid，也可实现折叠，但需要将 A 转化为存在 Monoid 的类型 B
    */
  def foldMap[A, B](xs: List[A], m: Monoid[B])(f: A ⇒ B): B = xs.foldLeft(m.zero)((b, a) ⇒ m.op(f(a), b))

  def foldMap_2[A, B](xs: List[A], m: Monoid[B])(f: A ⇒ B): B = (xs map f).foldLeft(m.zero)(m.op)

  /**
    * Exercise 10.6 foldMap 可以实现 foldLeft/foldRight 实现，其实 foldLeft/foldRight 也可以使用 foldMap 实现
    *
    * f 是 (A, B) => B 或者 (B, A) => B，可以将他们处理为 A => (B => B)，即使用 Monoid[B => B] 来折叠
    */
  def foldLeftViaFoldMap[A, B](z: B, xs: List[A])(f: (B, A) ⇒ B): B =
    foldMap(xs, endoMonoid: Monoid[B ⇒ B])(a ⇒ b ⇒ f(b, a))(z)

  def foldRightViaFoldMap[A, B](z: B, xs: List[A])(f: (A, B) ⇒ B): B =
    foldMap(xs, endoMonoid[B])(f.curried)(z)

  /**
    * Exercise 10.7 为 IndexedSeq 实现 foldMap，策略为将 IndexedSeq 分为两部分，递归处理，然后使用 monoid 合并两部分的结果
    */
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A ⇒ B): B =
    if (v.length <= 1) (v map f).headOption.getOrElse(m.zero)
    else {
      val (l, r) = v.splitAt(v.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  /**
    * Exercise 10.8 使用第七章的库实现并行版的 foldMap
    *
    * 思路：
    * 1. 实现一个组合子 par，将 Monoid[A] 提升为 Monoid[Par[A]]
    * 2. 使用 par 实现 parFoldMap
    */
  import com.satansk.fpinscala.parallelism.Par._

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(p1: Par[A], p2: Par[A]): Par[A] = map2(p1, p2)(m.op)
    def zero: Par[A] = unit(m.zero)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A ⇒ B): Par[B] = foldMapV(v, par(m))(asyncF(f))

  /**
    * Exercise 10.9 使用 foldMap 判断给定的 IndexedSeq[Int] 是否是有序的
    */

  /**
    * sortMonoid 保存到目前为止，最小元素、最大元素以及是否有序的 tuple
    */
  val sortMonoid = new Monoid[Option[(Int, Int, Boolean)]] {
    def op(a1: Option[(Int, Int, Boolean)], a2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] =
      (a1, a2) match {
        case (Some((x1, y1, t1)), Some((x2, y2, t2))) ⇒
          Some((x1 min x2, y1 max y2, t1 && t2 && y1 <= x2)) // 升序
        case (x, None)            ⇒ x
        case (None, x)            ⇒ x
      }
    def zero = None
  }

  def isSorted(xs: IndexedSeq[Int]): Boolean =
    foldMapV(xs, sortMonoid)(i ⇒ Some(i, i, true)).forall(_._3)

  /**
    * 以下 ADT（代数数据结构）表示单词计数的部分结果：
    *
    * 1. Stub 是最简单的形式，表示还没有看到任何完整的单词
    * 2. Part 保存看到的完整单词的个数，lStub 保存左边的部分单词，rStub 保存邮编的部分单词
    */
  sealed trait WC
  final case class Stub(chars: String) extends WC
  final case class Part(lStub: String, words: Int, rStub: String) extends WC

  /**
    * Exercise 10.10 为 WC 编写 monoid 实例，并确保满足 monoid 法则
    */
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a1: WC, a2: WC): WC =
      (a1, a2) match {
        case (Stub(s1), Stub(s2))     ⇒ Stub(s1 + s2)
        case (Stub(s), Part(l, w, r)) ⇒ Part(s + l, w, r)
        case (Part(l, w, r), Stub(s)) ⇒ Part(l, w, r + s)
        case (Part(l1, w1, r1), Part(l2, w2, r2)) ⇒ Part(l1, w1 + w2 + (if ((r1 + l2).isEmpty) 0 else 1), r2)
      }
    def zero: WC = Stub("")
  }

  /**
    * Exercise 10.11 使用 Monoid[WC] 实现 count 函数，递归拆分字符串，并计算各自包含的单词个数，最后再汇总
    *
    * 这体现了用 Monoid 解决问题的思路，String 对应的 stringMonoid 无法解决单词计数问题，所以我们创造了 WC 类型以及 wcMonoid，剩下的就是使用
    * foldMap 进行转换了。
    */
  def count(s: String): Int = {
    /**
      * Char => WC 转换函数
      */
    def aux(c: Char): WC =
      if (c.isWhitespace) Part("", 0, "")
      else Stub(c.toString)

    /**
      * Stub 不能含有空格，因此只能是空字符串 or 单词，unstub 对其计数，若为空字符串，则单词树为 0，否则为 1
      */
    def unstub(s: String): Int = s.length min 1

    /**
      * foldMapV 利用 Monoid 进行计算，非常抽象
      */
    foldMapV(s.toIndexedSeq, wcMonoid)(aux) match {
      case Stub(s)        ⇒ unstub(s)
      case Part(l, w, r)  ⇒ unstub(l) + w + unstub(r)
    }
  }

  /**
    * 1. 很多数据结构都可以使用 foldLeft/foldMap 等函数折叠，比如 List/Tree/Stream/IndexedSeq 等，将该特性抽象为 Foldable 特质
    * 2. 具体实现时，foldLeft/foldRight 和 foldMap 可以互相实现
    * 3. F[_] 是高阶类型（higher-kinder type），它接受一个类型参数
    */
  trait Foldable[F[_]] {
    def foldRight[A, B](xs: F[A])(z: B)(f: (A, B) ⇒ B): B = foldMap(xs)(f.curried)(endoMonoid)(z)
    def foldLeft[A, B](xs: F[A])(z: B)(f: (B, A) ⇒ B): B = foldMap[A, B ⇒ B](xs)(a ⇒ b ⇒ f(b, a))(endoMonoid)(z)
    def foldMap[A, B](xs: F[A])(f: A ⇒ B)(m: Monoid[B]): B = foldLeft(xs)(m.zero)((b, a) ⇒ m.op(f(a), b))
    def concatenate[A](xs: F[A])(m: Monoid[A]): A = foldLeft(xs)(m.zero)(m.op)
    /**
      * Exercise 10.15 编写通过的转化方法，将 Foldable 结构转化为 List
      */
    def toList[A](fa: F[A]): List[A] = foldRight(fa)(Nil: List[A])(_ :: _)
  }

  /**
    * Exercise 10.12 实现 Foldable[List]、Foldable[IndexedSeq] 和 Foldable[Stream]
    *
    * 注意：foldLeft/foldRight 和 foldMap 之间可以互相实现
    */
  object ListFoldable extends Foldable[List] {
    override def foldRight[A, B](xs: List[A])(z: B)(f: (A, B) ⇒ B): B = xs.foldRight(z)(f)
    override def foldLeft[A, B](xs: List[A])(z: B)(f: (B, A) ⇒ B): B = xs.foldLeft(z)(f)
    override def foldMap[A, B](xs: List[A])(f: (A) ⇒ B)(m: Monoid[B]): B = foldRight(xs)(m.zero)((a, b) ⇒ m.op(f(a), b))
  }

  object IndexedSeqFoldable extends Foldable[IndexedSeq] {
    override def foldRight[A, B](xs: IndexedSeq[A])(z: B)(f: (A, B) ⇒ B): B = xs.foldRight(z)(f)
    override def foldLeft[A, B](xs: IndexedSeq[A])(z: B)(f: (B, A) ⇒ B): B = xs.foldLeft(z)(f)
    override def foldMap[A, B](xs: IndexedSeq[A])(f: (A) ⇒ B)(m: Monoid[B]): B = foldMapV(xs, m)(f)
  }

  object StreamFoldable extends Foldable[Stream] {
    override def foldRight[A, B](xs: Stream[A])(z: B)(f: (A, B) ⇒ B): B = xs.foldRight(z)(f)
    override def foldLeft[A, B](xs: Stream[A])(z: B)(f: (B, A) ⇒ B): B = xs.foldLeft(z)(f)
  }

  /**
    * Exercise 10.13 为第三章实现的二叉树实现 Foldable 实例
    *
    * 此处 foldMap 实现中，并未用到 m.zero，因为实际中并没有空树；这提示我们，似乎可以对比 Monoid 更小的结构进行折叠，即无 zero，只有
    * 可结合的 op 操作，这种没有 zero 的结构被称为 semigroup
    */
  object TreeFoldable extends Foldable[Tree] {
    override def foldRight[A, B](xs: Tree[A])(z: B)(f: (A, B) ⇒ B): B =
      xs match {
        case Leaf(v)      ⇒ f(v, z)
        case Branch(l, r) ⇒ foldRight(r)(foldRight(l)(z)(f))(f)
      }
    override def foldLeft[A, B](xs: Tree[A])(z: B)(f: (B, A) ⇒ B): B =
      xs match {
        case Leaf(v)      ⇒ f(z, v)
        case Branch(l, r) ⇒ foldLeft(l)(foldLeft(r)(z)(f))(f)
      }
    override def foldMap[A, B](xs: Tree[A])(f: (A) ⇒ B)(m: Monoid[B]): B =
      xs match {
        case Leaf(v)      ⇒ f(v)
        case Branch(l, r) ⇒ m.op(foldMap(l)(f)(m), foldMap(r)(f)(m))
      }
  }

  /**
    * Exercise 10.14 实现 Foldable[Option] 实例
    */
  object OptionFoldable extends Foldable[Option] {
    override def foldRight[A, B](xs: Option[A])(z: B)(f: (A, B) ⇒ B): B =
      xs match {
        case None     ⇒ z
        case Some(a)  ⇒ f(a, z)
      }
    override def foldLeft[A, B](xs: Option[A])(z: B)(f: (B, A) ⇒ B): B =
      xs match {
        case None     ⇒ z
        case Some(a)  ⇒ f(z, a)
      }
    override def foldMap[A, B](xs: Option[A])(f: (A) ⇒ B)(m: Monoid[B]): B =
      xs match {
        case None     ⇒ m.zero
        case Some(a)  ⇒ f(a)
      }
  }

  /**
    * Exercise 10.16 若类型 A 和 类型 B 是 Monoid，则类型 (A, B) 也是 Monoid，实现 Monoid[(A, B)]
    */
  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      override def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
      override def zero: (A, B) = (A.zero, B.zero)
    }

  /**
    * 示例 10.1 合并 key-value Map
    *
    * 只要包含的元素是 Monoid，某些数据类型就能构建成 Monoid
    */
  def mapMergeMonoid[K, V](m: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      override def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] =
        (a1.keySet ++ a2.keySet).foldRight(zero) {
          (k, map) ⇒ map.updated(k, m.op(a1.getOrElse(k, m.zero), a2.getOrElse(k, m.zero)))
        }
      override def zero: Map[K, V] = Map()
    }

  /**
    * Exercise 10.17 为返回 Monoid 的函数编写 Monoid 实例
    */
  def functionMonoid[A, B](m: Monoid[B]): Monoid[A ⇒ B] =
    new Monoid[A ⇒ B] {
      override def op(f1: A ⇒ B, f2: A ⇒ B): A ⇒ B = a ⇒ m.op(f1(a), f2(a))
      override def zero: A ⇒ B = a ⇒ m.zero
    }

  /**
    * Exercise 10.18 实现 bag 函数，输入一个集合，返回 Map 中，key 为集合中的元素，value 为元素出现的次数
    */
  def bag[A](xs: IndexedSeq[A]): Map[A, Int] =
    foldMapV(xs, mapMergeMonoid[A, Int](intAddition))(a ⇒ Map(a → 1))

  def bag_2[A](xs: IndexedSeq[A]): Map[A, Int] =
    xs.map(a ⇒ Map(a → 1)).foldLeft(Map.empty[A, Int])(mapMergeMonoid(intAddition).op)

}
