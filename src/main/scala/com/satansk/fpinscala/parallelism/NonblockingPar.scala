package com.satansk.fpinscala.parallelism

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.AtomicReference

import com.sun.glass.ui.MenuItem.Callback

/**
  * Author:  satansk
  * Email:   satansk@hotmail.com
  * Date:    17/12/22
  */
object NonblockingPar {

  /**
    * 1. 实现 non-blocking Par 的关键是从 Future 获取结果的同时，不阻塞当前线程，而 java.concurrent.Future 的实现，
    *    决定了 Future.get 必然会阻塞当前线程，所以只能使用自定义的 Future
    * 2. 下面 Future 的 apply 方法，会接受一个 A => Unit 的回调函数，当 Future 的值准备好后，该函数被回调，处理结果值
    * 3. 使用回调函数替换 get 方法，避免阻塞
    */
  sealed trait Future[A] {
    private [parallelism] def apply(callback: A ⇒ Unit): Unit
  }

  /**
    * Par 的类型与之前一样，唯一的区别是此处使用自定义的 Future
    */
  type Par[+A] = ExecutorService ⇒ Future[A]

  /**
    * 1. run 实现中，latch.await 将阻塞调用线程，这里的阻塞是无法避免的，因为 run 要返回 A，则其必须等待该值可用后才能返回，巧妇难为无米之炊
    * 2. 希望用户避免调用 run，除非他们确实想要等待该值
    */
  def run[A](es: ExecutorService)(pa: Par[A]): A = {
    /**
      * 1. AtomicReference 可变但线程安全的引用，用于存储计算结果值
      * 2. CountDownLatch 是一种同步工具类，可以阻塞线程，直到到达 CountDownLatch 的终止状态（计数器为 0）
      */
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)

    /**
      * 1. {} 是回调函数，当 pa(es) 获取的 Future 计算结束后，会调用该回调函数处理计算结果值
      * 2. 回调函数首先将计算结果 a 存储到 AtomicReference 中，然后调用 CountDownLatch.countDown 对计数器减一，使其马上到达终止状态
      */
    pa(es) {
      a ⇒ {
        ref.set(a)
        latch.countDown()
      }
    }

    /**
      * 1. CountDownLatch.await 将阻塞当前线程，直到计数器为 0，当 Future 计算完成，回调函数中的 CountDownLatch.countDown 执行后，
      *    计数器变为 0，CountDownLatch 到达终止状态，当前线程将恢复执行
      * 2. 线程结束阻塞户，将获取 AtomicReference 中存储的 Future 的计算结果，作为 run 函数的计算结果
      */
    latch.await()
    ref.get
  }

  /**
    * 1. 直接将 a 传递给回调函数
    * 2. unit 实现其实没有用到 ExecutorService
    */
  def unit[A](a: A): Par[A] =
    _ ⇒ new Future[A] {
      def apply(callback: A ⇒ Unit): Unit = callback(a)
    }

  def fork[A](a: ⇒ Par[A]): Par[A] =
    es ⇒ new Future[A] {
      def apply(callback: (A) ⇒ Unit): Unit =
        eval(es)(a(es)(callback))
    }

  def eval(es: ExecutorService)(r: ⇒ Unit): Unit =
    es.submit(new Callable[Unit] {
      def call = r
    })

}
