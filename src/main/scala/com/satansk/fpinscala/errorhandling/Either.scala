package com.satansk.fpinscala.errorhandling

/**
  * Author:  satansk
  * Email:   satansk@hotmail.com
  * Date:    17/10/13
  */
sealed trait EitherT[+E, +A] {

  /**
    * Exercise 4.6 实现 Either 版本的 `map` `flatMap` `map2` `sequence` 函数
    */
  def map[B](f: A ⇒ B): EitherT[E, B] = this match {
    case Right(v) ⇒ Right(f(v))
    case Left(e)  ⇒ Left(e)
  }

  def flatMap[EE >: E, B](f: A ⇒ EitherT[EE, B]): EitherT[EE, B] = this match {
    case Right(v) ⇒ f(v)
    case Left(e)  ⇒ Left(e)
  }

  def orElse[EE >: E, B >: A](b: ⇒ EitherT[EE, B]): EitherT[EE, B] = this match {
    case Left(_)  ⇒ b
    case Right(v) ⇒ Right(v)
  }

  def map2[EE >: E, B, C](b: EitherT[EE, B])(f: (A, B) ⇒ C): EitherT[EE, C] = this flatMap(aa ⇒ b map (bb ⇒ f(aa, bb)))

  def map2ViaFor[EE >: E, B, C](b: EitherT[EE, B])(f: (A, B) ⇒ C): EitherT[EE, C] =
    for {
      aa ← this
      bb ← b
    } yield f(aa, bb)

}

case class Left[+E](v: E) extends EitherT[E, Nothing]
case class Right[+A](v: A) extends EitherT[Nothing, A]

object EitherT {

  def mean(xs: List[Double]): EitherT[String, Double] =
    if (xs.isEmpty) Left("mean of empty list")
    else Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): EitherT[Exception, Double] = Try(x / y)

  /**
    * Exercise 4.7 实现 Either 的 `sequence` 和 `traverse` 函数，若遇到错误，则返回第一个错误
    */
  def sequence[E, A](xs: List[EitherT[E, A]]): EitherT[E, List[A]] =
    xs.foldRight(Right(Nil): EitherT[E, List[A]])((h, t) ⇒ (h map2 t) (_ :: _))

  def traverse[E, A, B](xs: List[A])(f: A ⇒ EitherT[E, B]): EitherT[E, List[B]] =
    xs.foldRight(Right(Nil): EitherT[E, List[B]])((h, t) ⇒ (f(h) map2 t) (_ :: _))

  def sequence2[E, A](xs: List[EitherT[E, A]]): EitherT[E, List[A]] = traverse(xs)(x ⇒ x)

  def traverse2[E, A, B](xs: List[A])(f: A ⇒ EitherT[E, B]): EitherT[E, List[B]] = xs match {
    case Nil  ⇒ Right(Nil)
    case h :: t ⇒ (f(h) map2 traverse2(t)(f))(_ :: _)
  }

  /**
    * Exercise 4.7
    *
    * There are a number of variations on `Option` and `Either`. If we want to accumulate multiple errors, a simple
    * approach is a new data type that lets us keep a list of errors in the data constructor that represents failures:
    *
    *  trait Partial[+A,+B]
    *  case class Errors[+A](get: Seq[A]) extends Partial[A,Nothing]
    *  case class Success[+B](get: B) extends Partial[Nothing,B]
    *
    * There is a type very similar to this called `Validation` in the Scalaz library. You can implement `map`, `map2`,
    * `sequence`, and so on for this type in such a way that errors are accumulated when possible (`flatMap` is unable to
    * accumulate errors--can you see why?). This idea can even be generalized further--we don't need to accumulate failing
    * values into a list; we can accumulate values using any user-supplied binary function.
    *
    * It's also possible to use `Either[List[E],_]` directly to accumulate errors, using different implementations of
    * helper functions like `map2` and `sequence`.
    */

  def Try[A](a: ⇒ A): EitherT[Exception, A] =
    try Right(a)
    catch { case e: Exception ⇒ Left(e)}
}