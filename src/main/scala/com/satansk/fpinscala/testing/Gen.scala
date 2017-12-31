package com.satansk.fpinscala.testing


import java.util.concurrent.{ExecutorService, Executors}

import com.satansk.fpinscala.state.RNG
import com.satansk.fpinscala.state.RNG._
import com.satansk.fpinscala.state.State
import com.satansk.fpinscala.laziness.Stream
import com.satansk.fpinscala.parallelism.Par.{Par, map2}
import com.satansk.fpinscala.testing.Prop._

/**
  * Author:  satansk
  * Email:   satansk@hotmail.com
  * Date:    17/12/24
  */

case class Prop(run: (MaxSize, TestCases, RNG) ⇒ Result) {

  def check: Option[(FailedCase, SuccessCount)] = ???

  /**
    * Exercise 8.3 & 8.9 实现 && 函数和 || 函数
    *
    * 思路：&& 和 || 都需先运行 this.run 然后根据结果决定是否运行 p.run
    */
  def &&(p: Prop): Prop = Prop {
    (max, n, rng) ⇒ run(max, n, rng) match {
      case Passed | Proved  ⇒ p.run(max, n, rng)
      case failed           ⇒ failed
    }
  }

  /**
    * 1. 当两边 Prop 都失败时，需要在第二个 Prop 结果上打 tag，标注第一个 Prop 失败的提示信息
    * 2. || 并非区分 Passed 和 Proved 两种状态，只有失败 or 非失败
    */
  def ||(p: Prop): Prop = Prop {
    (max, n, rng) ⇒ run(max, n, rng) match {
      case Falsified(msg, _)  ⇒ p.tag(msg).run(max, n, rng)
      case x                  ⇒ x
    }
  }

  /**
    * 仅是对调用的 Prop 的简单封装，在运行失败时添加指定的 msg 提示信息
    */
  def tag(msg: String): Prop = Prop {
    (max, n, rng) ⇒ run(max, n, rng) match {
      case Passed           ⇒ Passed
      case Falsified(m, e)  ⇒ Falsified(msg + "\n" + m, e)
    }
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  /**
    * 1. sum pattern type: A result is Passed or Falsified.
    * 2. sum pattern type 使用 sealed trait + final case class 实现
    */
  sealed trait Result {
    def isFalsified: Boolean
  }

  final case object Passed extends Result {
    override def isFalsified: Boolean = false
  }

  final case object Proved extends Result {
    override def isFalsified: Boolean = false
  }

  final case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    override def isFalsified: Boolean = true
  }

  def forAll[A](g: SGen[A])(p: A ⇒ Boolean): Prop = forAll(g.forSize)(p)

  def forAll[A](g: Int ⇒ Gen[A])(p: A ⇒ Boolean): Prop = Prop {
    (max, n, rng) ⇒ {
      val casesPerSize = (max - 1 + n) / max
      val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i ⇒ forAll(g(i))(p))
      val prop: Prop = props.map(p ⇒ Prop {
        (max, _, rng) ⇒ p.run(max, casesPerSize, rng)
      }).toList.reduce(_ && _)
      prop.run(max, n, rng)
    }
  }

  def forAll[A](g: Gen[A])(p: A ⇒ Boolean): Prop = Prop {
    (max, n, rng) ⇒  randomStream(g)(rng).zip(Stream.from(0)).take(n).map {
      case (a, index) ⇒ try {
        if (p(a)) Passed
        else Falsified(a.toString, index)
      } catch {
        case e: Exception ⇒ Falsified(buildMsg(a, e), index)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng ⇒ Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  /**
    * 运行 Prop.run 的辅助函数
    *
    * 1. 为 3 个参数提供默认值，调用方便
    * 2. 执行成功后，将结果打印到控制台
    */
  def run(p: Prop, maxSize: MaxSize = 100, testCases: TestCases = 100, rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Passed             ⇒ println(s"+ OK, passed $testCases tests.")
      case Proved             ⇒ println(s"+ OK, proved property.")
      case Falsified(msg, n)  ⇒ println(s"! Falsified after $n tests:\n $msg")
    }

  /**
    * 1. Prop 主构造函数接受一个 (MaxSize, TestCases, RNG) ⇒ Result 函数作为参数，因为只有一个参数，所以可以用 {} 替代 ()
    * 2. 简单将 3 个参数忽略，仅根据 p 返回成功或失败
    */
  def check(p: ⇒ Boolean): Prop = Prop {
    (_, _, _) ⇒ if (p) Proved else Falsified("()", 0)
  }

  /**
    * 专门用于测试 Par
    */
  def checkPar(p: ⇒ Par[Boolean]): Prop = forAllPar(Gen.unit(()))(_ ⇒ p)

  /**
    * 将比较过程推迟到实际运行 Par 时
    */
  def equal[A](p1: Par[A], p2: Par[A]): Par[Boolean] = map2(p1, p2)(_ == _)

  /**
    * S 类型为 Gen[ExecutorService]，可以根据不同比重提供两种线程池：定长 & 不定长
    */
  val S: Gen[ExecutorService] = Gen.weighted(
    Gen.choose(1, 4).map(Executors.newFixedThreadPool) → 0.75,
    Gen.unit(Executors.newCachedThreadPool) → 0.25
  )

  /**
    * 使用 S.map2(g)((_, _)) 组合两个 Gen，非常臃肿
    */
  def forAllPar_1[A](g: Gen[A])(f: A ⇒ Par[Boolean]): Prop =
    forAll(S.map2(g)((_, _))) {
      case (s, a) ⇒ f(a)(s).get
    }

  /**
    * 使用 ** 函数，可读性大大提升
    */
  def forAllPar_2[A](g: Gen[A])(f: A ⇒ Par[Boolean]): Prop =
    forAll(S ** g) {
      case (s, a) ⇒ f(a)(s).get
    }

  /**
    * 使用 ** 抽取器，进一步优化
    */
  def forAllPar[A](g: Gen[A])(f: A ⇒ Par[Boolean]): Prop =
    forAll(S ** g) {
      case s ** a ⇒ f(a)(s).get
    }

}

case class SGen[A](forSize: Int ⇒ Gen[A])

case class Gen[A](sample: State[RNG, A]) {

  def map[B](f: A ⇒ B): Gen[B] = Gen(sample.map(f))

  def map2_1[B, C](g: Gen[B])(f: (A, B) ⇒ C): Gen[C] =
    Gen(sample.flatMap(a ⇒ g.sample.map(b ⇒ f(a, b))))

  def map2[B, C](g: Gen[B])(f: (A, B) ⇒ C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def **[B](g: Gen[B]): Gen[(A, B)] =
    this.map2(g)((_, _))

  /**
    * Exercise 8.6 实现 flatMap 函数，然后使用 flatMap 实现更动态的 listOfN
    */
  def flatMap[B](f: A ⇒ Gen[B]): Gen[B] =
    Gen(sample.flatMap(a ⇒ f(a).sample))

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n ⇒ listOfN(n))

  /**
    * Exercise 8.10 实现 unsized 函数，将生成器 Gen 转化为定长生成器 SGen
    */
  def unsized: SGen[A] = SGen(_ ⇒ this)

}

object Gen {

  /**
    * Exercise 8.4 实现 choose 方法，生成指定范围内的整数（不含右边界值）
    */
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(nonNegativeInt).map(n ⇒ start + n % (stopExclusive - start)))

  /**
    * Exercise 8.5 实现 unit boolean 和 listOfN 函数
    */
  def unit[A](a: ⇒ A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

  /**
    * 生成指定长度 n 的列表
    */
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  /**
    * Exercise 8.7 实现 union 函数，将两个同类型的生成器组合成一个
    */
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b ⇒ if (b) g1 else g2)

  /**
    * Exercise 8.8 实现 weighted 函数，类似 union，但根据权重从不同的生成器中获取不同的值
    */
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Threshold = g1._2 / (g1._2 + g2._2)
    Gen(State(RNG.double)).flatMap(d ⇒ if (d > g1Threshold) g1._1 else g2._1)
  }

  /**
    * Exercise 8.12 实现无 size 参数的 listOf 组合子
    */
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(g.listOfN)

  /**
    * Exercise 8.13 实现 listOf1 函数，生成非空列表
    */
  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n ⇒ g.listOfN(n max 1))
}

/**
  * 定义有 unapply 方法的 ** 对象，使 ** 对象变成一个模式，可以用于模式匹配中
  */
object ** {
  def unapply[A, B](p: (A, B)): Option[(A, B)] = Some(p)
}
