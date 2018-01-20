package com.satansk.fpinscala.applicative

import com.satansk.fpinscala.monads.Functor

import scala.language.higherKinds

/**
  * Author:  satansk
  * Email:   satansk@hotmail.com
  * Date:    18/1/19
  */

/**
  * 1. Monad 中，原始组合子是 unit + flatMap，而 map2 由这两者实现
  * 2. 实际上，很多组合子都可以使用 unit + map2 实现，可不再使用 flatMap 实现 map2，而是将 map2 实现为 primitive combinator，
  *    由此获取另外一个抽象：可应用函子，其 primitive combinators 为：unit + map2
  */
trait Applicative[F[_]] extends Functor[F] {
  // primitive combinators
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) ⇒ C): F[C]
  def unit[A](a: ⇒ A): F[A]

  // derived combinators
  /**
    * 所有 Applicative 都是 Functor，因为可以用 unit + map2 实现 map，所以索性使 Applicative 继承 Functor
    */
  override def map[A, B](fa: F[A])(f: (A) ⇒ B): F[B] =
    map2(fa, unit(()))((a, _) ⇒ f(a))

  def traverse[A, B](xs: List[A])(f: A ⇒ F[B]): F[List[B]] =
    xs.foldRight(unit[List[B]](Nil))((a, l) ⇒ map2(f(a), l)(_ :: _))

  /**
    * Exercise 12.1 尽量将 Monad 的组合子移到 Applicative 中，并使用 unit + map2 实现（或其他使用 unit + map2 实现的函数）
    */
  def sequence[A](fxs: List[F[A]]): F[List[A]] =
    traverse(fxs)(fa ⇒ fa)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  /**
    * Exercise 12.2 使用 unit + map2 实现 apply；使用 unit + apply 实现 map、map2
    */
  def apply[A, B](fab: F[A ⇒ B])(fa: F[A]): F[B] =
    map2(fab, fa)(_(_))  // (_(_)) 等价于 (f, x) => f(x)

  def mapViaApply[A, B](fa: F[A])(f: A ⇒ B): F[B] =
    apply(unit(f))(fa)

  def map2ViaApply[A, B, C](fa: F[A], fb: F[B])(f: (A, B) ⇒ C): F[C] =
    apply(apply(unit(f.curried))(fa))(fb)

  def map2ViaApply_2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) ⇒ C): F[C] =
    apply(map(fa)(f.curried))(fb)

  /**
    * Exercise 12.3 使用 unit + apply + 柯里化实现 map3、map4 函数
    */
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) ⇒ D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) ⇒ E): F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

}

/**
  * 因为 flatMap 可以实现 map2，所以如果让 Monad 继承 Applicative，则 Monad 仅需提供 flatMap 的实现即可获取 Applicative 所有能力
  */
trait Monad[F[_]] extends Applicative[F] {
  /**
    * flatMap = map + join/flatten
    */
  def flatMap[A, B](fa: F[A])(f: A ⇒ F[B]): F[B] = join(map(fa)(f))

  def join[A](ffa: F[F[A]]): F[A] =
    flatMap(ffa)(fa ⇒ fa)

  def compose[A, B, C](f: A ⇒ F[B])(g: B ⇒ F[C]): A ⇒ F[C] =
    a ⇒ flatMap(f(a))(g)

  override def map[A, B](fa: F[A])(f: A ⇒ B): F[B] =
    flatMap(fa)(a ⇒ unit(f(a)))

  /**
    * map2 = flatMap + map
    */
  def map2[A, B, C](fa: F[A])(fb: F[B])(f: (A, B) ⇒ C): F[C] =
    flatMap(fa)(a ⇒ map(fb)(b ⇒ f(a, b)))
}
