package com.satansk.fpinscala.applicative

import java.util.Date

import com.satansk.fpinscala.monads.Functor

import scala.language.higherKinds
import scala.language.reflectiveCalls

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

  /**
    * Exercise 12.8 为 Applicative Functor 实现 product 函数
    */
  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self: Applicative[F] = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      // f[A] f[B] => F[C]
      // 此处 IDEA 报错系误报
      override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) ⇒ C): (F[C], G[C]) =
        (self.map2(fa._1, fb._1)(f), G.map2(fa._2, fb._2)(f))

      override def unit[A](a: ⇒ A): (F[A], G[A]) = (self.unit(a), G.unit(a))
    }
  }

  /**
    * Exercise 12.9 实现 compose 函数（若 F[_] 和 G[_] 是函子，则 F[G[_]] 也是函子）
    */
  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[G[x]])})#f] = {
    val self: Applicative[F] = this
    new Applicative[({type f[x] = F[G[x]]})#f] {
      // 此处 IDEA 报错系误报
      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) ⇒ C): F[G[C]] =
        self.map2(fa, fb)(G.map2(_, _)(f))

      override def unit[A](a: ⇒ A): F[G[A]] = self.unit(G.unit(a))
    }
  }

}

object Applicative {
  /**
    * 与 Either 类似，但可以显示处理多个错误
    */
  sealed trait Validation[+E, +A]
  final case class Failure[E](hd: E, tl: Vector[E] = Vector()) extends Validation[E, Nothing]
  final case class Success[A](a: A) extends Validation[Nothing, A]

  /**
    * Exercise 12.6 为 Validation 实现 Applicative 实例，累计失败时的错误（失败时，至少有一个错误保存在 hd 上，其余保存在 tl 上）
    */
  def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] =
    new Applicative[({type f[x] = Validation[E, x]})#f] {
      def unit[A](a: ⇒ A): Validation[E, A] = Success(a)
      def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) ⇒ C): Validation[E, C] =
        (fa, fb) match {
          case (Success(a), Success(b))       ⇒ Success(f(a, b))
          case (Success(_), Failure(hd, tl))  ⇒ Failure(hd, tl)
          case (Failure(hd, tl), Success(_))  ⇒ Failure(hd, tl)
          case (Failure(h1, t1), Failure(h2, t2)) ⇒ Failure(h1, t1 ++ Vector(h2) ++ t2)
        }
    }

  /**
    * 示例 12.5 校验用户在表单上的输入
    */
  case class WebForm(name: String, birthdate: Date, number: String)

  def validName(name: String): Validation[String, String] =
    if (name != "") Success(name)
    else Failure("Name can't be empty")
  def validBirthdate(birthdate: String): Validation[String, Date] =
    try {
      import java.text._
      Success(new SimpleDateFormat("yyyy-MM-dd").parse(birthdate))
    } catch {
      case _: Exception ⇒ Failure("Birthdate must be in the form yyyy-MM-dd")
    }
  def validPhone(number: String): Validation[String, String] =
    if (number.matches("[0-9]{10}")) Success(number)
    else Failure("Phone number must be 10 digits")

  // 若要一次验证 3 个表单的输入，可以使用 map3
  def validForm(name: String, birthdate: String, number: String): Validation[String, WebForm] =
    validationApplicative.map3(
      validName(name),
      validBirthdate(birthdate),
      validPhone(number)
    )(WebForm)

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
  def map2_2[A, B, C](fa: F[A])(fb: F[B])(f: (A, B) ⇒ C): F[C] =
    flatMap(fa)(a ⇒ map(fb)(b ⇒ f(a, b)))
}

object Monad {
  /**
    * Exercise 12.5 为 Either 实现 Monad 实例
    */
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def flatMap[A, B](fa: Either[E, A])(f: (A) ⇒ Either[E, B]): Either[E, B] =
      fa match {
        case Right(a) ⇒ f(a)
        case Left(e)  ⇒ Left(e)
      }
    override def map2[A, B, C](fa: Either[E, A], fb: Either[E, B])(f: (A, B) ⇒ C): Either[E, C] =
      (fa, fb) match {
        case (Right(a), Right(b)) ⇒ Right(f(a, b))
        case (Right(_), Left(e))  ⇒ Left(e)
        case (Left(e), _)         ⇒ Left(e)
      }
    override def unit[A](a: ⇒ A): Either[E, A] = Right(a)
  }

}