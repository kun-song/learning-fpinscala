package com.satansk.fpinscala.state

/**
  * Author:  satansk
  * Email:   satansk@hotmail.com
  * Date:    17/10/23
  */
trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val nextSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(nextSeed)
      val n = (nextSeed >>> 16).toInt

      (n, nextRNG)
    }
  }

  // v1 == v2
  def randomPairSame(rng: RNG): (Int, Int) = {
    val (v1, _) = rng.nextInt
    val (v2, _) = rng.nextInt
    (v1, v2)
  }

  // v1 != v2
  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (v1, r1) = rng.nextInt
    val (v2, r2) = r1.nextInt
    ((v1, v2), r2) // 返回 r2
  }

  /**
    * Exercise 6.1 使用 RNG.nextInt 生成 [0, Int.MaxValue] 之间的随机数，注意 Int.MinValue 不应该返回负值
    *
    * 1. Int 范围是 [-2147483648, 2147483647]，生成结果应该是 [0, 2147483647]
    * 2. 除 Int.MinValue 和 0 外，其他数值 |n| 出现两次
    */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (v, r) = rng.nextInt
    val n = if (v == Int.MinValue) 0 else v
    (n.abs, r)
  }

  /**
    * Exercise 6.2 生成 [0, 1) 之间的 Double 数，不包含 1
    */
  def double(rng: RNG): (Double, RNG) = {
    val (v, r) = nonNegativeInt(rng)
    (v / (Int.MaxValue.toDouble + 1), r) // Int.MaxValue + 1 会溢出
  }

  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match {
      case (i, r) ⇒ (i % 2 == 0, r)
    }

  /**
    * Exercise 6.3 实现 `intDouble` `doubleInt` `double3`
    */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  /**
    * Exercise 6.4 实现 `ints`，生成一组随机整数
    */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def aux(n: Int)(rng: RNG)(acc: List[Int]): (List[Int], RNG) =
      (n, rng.nextInt) match {
        case (0, _) ⇒ (acc, rng)
        case (x, (i, r)) ⇒ aux(x - 1)(r)(i :: acc)
      }

    aux(count)(rng)(Nil)
  }

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) =
    (count, rng.nextInt) match {
      case (0, _) ⇒ (Nil, rng)
      case (n, (i, r)) ⇒
        val (ll, rr) = ints(n - 1)(r)
        (i :: ll, rr)
    }

  type Rand[+A] = RNG ⇒ (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng ⇒ (a, rng)

  def map[A, B](s: Rand[A])(f: A ⇒ B): Rand[B] =
    rng ⇒ {
      val (a, r) = s(rng)
      (f(a), r)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i ⇒ i - i % 2)

  /**
    * Exercise 6.5 使用 `map` 实现更优雅的 `double`
    */
  def doubleViaMap: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  /**
    * Exercise 6.6 实现 `map2`
    */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) ⇒ C): Rand[C] =
    rng ⇒ {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val intDouble2: Rand[(Int, Double)] = both(int, double)
  val doubleInt2: Rand[(Double, Int)] = both(double, int)

  /**
    * Exercise 6.7 实现 `sequence` 函数
    */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng ⇒ {
      val la = fs.foldRight((Nil: List[A], rng))((rand, lr) ⇒ {
        val (a, r) = rand(lr._2)
        (a :: lr._1, r)
      })

      la
    }

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((a, acc) ⇒ map2(a, acc)(_ :: _))

  def intsViaSequence(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  /**
    * Exercise 6.8 实现 `flatMap`
    */
  def flatMap[A, B](f: Rand[A])(g: A ⇒ Rand[B]): Rand[B] =
    rng ⇒ {
      val (a, r) = f(rng)
      g(a)(r)
    }

  // 若 i + (n - 1) - mod >= 0，则重新生成
  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i ⇒ {
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    })

  /**
    * Exercise 6.9 使用 `flatMap` 实现 `map` `map2`
    */
  def mapViaFlatMap[A, B](ra: Rand[A])(f: A ⇒ B): Rand[B] = flatMap(ra)(a ⇒ rng ⇒ (f(a), rng))

  // rng => (a, rng) is unit(a)
  def mapViaFlatMap2[A, B](ra: Rand[A])(f: A ⇒ B): Rand[B] = flatMap(ra)(a ⇒ unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) ⇒ C): Rand[C] =
    flatMap(ra)(a ⇒ rng ⇒ {
      val (b, r) = rb(rng)
      (f(a, b), r)
    })

  def map2ViaFlatMap2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) ⇒ C): Rand[C] =
    flatMap(ra)(a ⇒ map(rb)(b ⇒ f(a, b)))

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

}

// 在 State 类中使用 object State 的函数，则需要该行
import State._

/**
  * map flatMap 等与 RNG 状态解耦，是处理 state action 的通用函数
  */
case class State[S, +A](run: S ⇒ (A, S)) {

  /**
    * Exercise 6.9 实现泛化的 `flatMap` `map` `map2` `unit` `sequence` 函数
    */
  def flatMap[B](f: A ⇒ State[S, B]): State[S, B] =
    State(S ⇒ {
      val (a, s) = this.run(S)
      f(a).run(s)
    })

  def map[B](f: A ⇒ B): State[S, B] = flatMap(a ⇒ unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) ⇒ C): State[S, C] = flatMap(a ⇒ sb.map(b ⇒ f(a, b)))

}

object State {

  def unit[S, A](a: A): State[S, A] = State(S ⇒ (a, S))

  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] =
    ss.foldRight(unit(Nil): State[S, List[A]])((sa, sl) ⇒ sa.map2(sl)(_ :: _))
}

