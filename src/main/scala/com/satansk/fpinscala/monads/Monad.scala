package com.satansk.fpinscala.monads

import com.satansk.fpinscala.laziness.Empty
import com.satansk.fpinscala.parallelism.Par
import com.satansk.fpinscala.parallelism.Par.Par
import com.satansk.fpinscala.testing.Gen

import scala.language.higherKinds

/**
  * Author:  satansk
  * Email:   satansk@hotmail.com
  * Date:    18/1/10
  */
/**
  * 1. F[_] 为高阶类型，即接受一个类型作为参数，并构造出另一个类型，而 F 本身是类型构造器，并非具体类型；
  * 2. 例如 List 就是一个类型构造器，而不是类型，没有类型为 List 的值，只有类型为 List[Int] 的值，而 List[Int] 就是构造器 List 接受 Int 类型
  *    进而构造出来的；
  */
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A ⇒ B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))
  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] =
    e match {
      case Right(fb) ⇒ map(fb)(Right(_))
      case Left(fa)  ⇒ map(fa)(Left(_))
    }
}

object Functor {

}

/**
  * 1. Monad 默认提供了 map 实现，因此它是一个 Functor，所以可以继承 Functor
  * 2. 所有的 Monad 都是 Functor，反之则不对
  *
  * Monad 并提供 map map2 方法的默认实现，所有 Monad 实例只要实现 unit flatMap 两个方法，就能获取 map/map2 方法，我们再也不需要为每个
  * 实现都实现一遍 map/map2 了。
  */
trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: ⇒ A): F[A]
  def flatMap[A, B](fa: F[A])(f: A ⇒ F[B]): F[B]

  def map[A, B](fa: F[A])(f: A ⇒ B): F[B] =
    flatMap(fa)(a ⇒ unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a ⇒ map(fb)(b ⇒ f(a, b)))
}

object Monad {

  val genMonad: Monad[Gen] = new Monad[Gen] {
    def unit[A](a: ⇒ A): Gen[A] = Gen.unit(a)
    def flatMap[A, B](ga: Gen[A])(f: A ⇒ Gen[B]): Gen[B] = ga flatMap f
  }

  /**
    * Exercise 11.1 为 Par、Parser、Option、Stream 和 List 编写 Monad 实例
    */
  val parMonad: Monad[Par] = new Monad[Par] {
    def flatMap[A, B](fa: Par[A])(f: (A) ⇒ Par[B]): Par[B] = flatMap(fa)(f)
    def unit[A](a: ⇒ A): Par[A] = Par.unit(a)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    def flatMap[A, B](fa: Option[A])(f: (A) ⇒ Option[B]): Option[B] = fa flatMap f
    def unit[A](a: ⇒ A): Option[A] = Some(a)
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    def flatMap[A, B](fa: Stream[A])(f: (A) ⇒ Stream[B]): Stream[B] = fa flatMap f
    def unit[A](a: ⇒ A): Stream[A] = Stream(a)
  }

  val listMonad: Monad[List] = new Monad[List] {
    def flatMap[A, B](fa: List[A])(f: (A) ⇒ List[B]): List[B] = fa flatMap f
    def unit[A](a: ⇒ A): List[A] = a :: Nil
  }

}