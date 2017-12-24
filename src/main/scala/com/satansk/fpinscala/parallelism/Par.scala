package com.satansk.fpinscala.parallelism

import java.util.concurrent._

/**
  * Author:  satansk
  * Email:   satansk@hotmail.com
  * Date:    17/10/30
  */
object Par {

  /**
    * 1. Par[A] 不是实际的数据类型，只是 ExecutorService ⇒ Future[A] 函数的类型别名
    * 2. 使用 Future 而非直接返回 A，可允许用户使用超时、取消等操作
    */
  type Par[A] = ExecutorService ⇒ Future[A]

  def unit[A](a: A): Par[A] = _ ⇒ UnitFuture(a)

  /**
    * Exercise 7.1 给出 map2 函数的签名
    *
    * 1. 将两个异步计算 Par[A] Par[B] 合并为一个异步计算 Par[C]，A B C 类型可以不同
    * 2. 注意不能用 val af = a(es).get;  val bf = b(es)，这会将两个异步计算串行化
    * 3. map2 并未考虑超时，仅将 ExecutorService 传递给 Par[A] 和 Par[B]，并 *等待* 这两个异步计算的结果，应用 f，最后将结果包装为 UnitFuture
    */
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) ⇒ C): Par[C] =
    es ⇒ {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  /**
    * 最简单自然的实现，为异步计算 Par[A] 分配一个新的执行任务（Callable 任务），任务由 ExecutorService 线程池中的线程实际执行，该实现问题如下：
    *
    * 1. 外部的 Callable 会阻塞，直到内部的任务完成，导致 Callable 任务将一直占用线程池中的线程，进而降低并行度
    * 2. 本质而言，一个线程即可完成的任务，实际占用了两个线程
    * 3. 注意（1）fork 的参数为 lazy parameter （2）fork 函数内部并未真正引用参数 a
    *    所以，在调用 fork 时不会立即对其参数求值，只有当调用 run 时才会真正执行 ExecutorService ⇒ Future[A] 函数，此时才会对参数 a 求值，从而实现惰性计算
    */
  def fork[A](a: ⇒ Par[A]): Par[A] =
    es ⇒ es.submit(new Callable[A] {
      def call(): A = a(es).get
    })

  def lazyUnit[A](a: ⇒ A): Par[A] = fork(unit(a))

  /**
    * Exercise 7.4 使用 lazyUnit 实现 asyncF 函数，将函数 A ⇒ B 转换为一个异步计算
    */
  def asyncF[A, B](f: A ⇒ B): A ⇒ Par[B] = a ⇒ lazyUnit(f(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  /**
    * 1. 通过函数签名很容易看出，map 是 map2 的简化版，只要忽略 map2 的第二个参数即可
    * 2. 当然可以使用 map2 类似方式实现 map，此处使用 map2，表明 map2 比 map 更加强大
    */
  def map[A, B](a: Par[A])(f: A ⇒ B): Par[B] = map2(a, unit(()))((a, _) ⇒ f(a))

  /**
    * 通过 map2 实现 sortPar
    */
  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map2(parList, unit(()))((a, _) ⇒ a.sorted)

  /**
    * 通过 map 实现 sortPar
    */
  def sortPar2(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  /**
    * 1. 并行处理列表 ps，将其转换为 Par[List]
    * 2. 使用已有的组合子实现 parMap
    */
  def parMap[A, B](ps: List[A])(f: A ⇒ B): Par[List[B]] = {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  /**
    * Exercise 7.5 实现 sequence 函数，将 List[Par] 转换为 Par[List]
    */

  /**
    * foldLeft 实现
    */
  def sequence_simple[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldLeft(unit(Nil: List[A]))((ph, pa) ⇒ map2(ph, pa)((l, a) ⇒ a :: l))

  /**
    * foldRight 实现，比 foldLeft 优点是可以用简写的 :: 函数
    */
  def sequence_simple2[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(Nil))((pa, pl) ⇒ map2(pa, pl)(_ :: _))


  /**
    * pattern match 实现
    *
    * 1. 优点：相比 foldLeft/foldRight 实现，优点是为每个 sequenceRight 调用分配一个新线程
    * 2. 缺点：各个线程的任务量不同，递增关系，不均匀
    */
  def sequenceRight[A](ps: List[Par[A]]): Par[List[A]] =
    ps match {
      case Nil    ⇒ unit(Nil)
      case h :: t ⇒ map2(h, fork(sequenceRight(t)))(_ :: _)
    }

  /**
    * sequenceRight 改进版
    *
    * 1. 采用二分法，各个线程任务量很均匀
    */
  def sequenceBalanced[A](ps: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (ps.isEmpty) unit(Vector())
    else if (ps.length == 1) map(ps.head)(a ⇒ Vector(a))
    else {
      val (l, r) = ps.splitAt(ps.length / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  /**
    * 最终版，内部由 sequenceBalanced 实现
    */
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = map(sequenceBalanced(ps.toIndexedSeq))(_.toList)

  /**
    * Exercise 7.6 实现 parFilter 函数，并行过滤列表元素
    *
    * 1. 并行由 asyncF 函数实现
    */
  def parFilter[A](xs: List[A])(f: A ⇒ Boolean): Par[List[A]] = {
    val xss: List[Par[List[A]]] = xs.map(asyncF(a ⇒ if (f(a)) a :: Nil else Nil))
    map(sequence(xss))(_.flatten)
  }

  def equal[A](es: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = p(es).get == p2(es).get

  /**
    * 实现必然是：首先等待计算 cond 的结果（必然阻塞），然后决定执行 t 还是 f
    */
  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es ⇒ {
      if (run(es)(cond).get) run(es)(t)
      else run(es)(f)
    }

  /**
    * Exercise 7.11 实现 choiceN 函数
    *
    * 泛化 1：既然能从两个可能中选择，则必然可以在 N 个可能中选择
    */
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es ⇒ {
      val index = run(es)(n).get
      run(es)(choices(index))
    }

  /**
    * choiceN 更加通用，更加强大
    */
  def choiceViaChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(c ⇒ if (c) 0 else 1))(t :: f :: Nil)

  /**
    * Exercise 7.12 实现 choiceMap 函数
    *
    * 泛化 2：不要过早限定在 List 容器上，这里其实与容器无关
    */
  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es ⇒ {
      val k = run(es)(key).get
      run(es)(choices(k))
    }

  /**
    * Exercise 7.13 实现 choicer 函数
    *
    * 泛化 3：choices 可以抽象为函数 A => Par[B]，在 choice 和 choiceN 中分别为 Boolean => Par[B] 和 Int => Par[B]
    */
  def choicer[A, B](pa: Par[A])(choices: A ⇒ Par[B]): Par[B] =
    es ⇒ {
      val a = run(es)(pa).get
      run(es)(choices(a))
    }

  /**
    * choicer 更加通用，可以轻松实现 choice choiceN
    */
  def choiceViaChoicer[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choicer(cond)(c ⇒ if (c) t else f)

  def choiceNViaChoicer[A](n: Par[Int])(xs: List[Par[A]]): Par[A] =
    choicer(n)(xs(_))

  /**
    * choicer 在很多函数式库中都存在，一般称为 bind 或者 flatMap
    */
  def flatMap[A, B](pa: Par[A])(choices: A ⇒ Par[B]): Par[B] =
    es ⇒ {
      val a = run(es)(pa).get
      run(es)(choices(a))
    }

  /**
    * flatMap 的行为分成两步：
    *
    * 1. map：在 Par[A] 上执行 map，根据 map 定义，结果为 Par[Par[A]]
    * 2. flatten：将 Par[Par[A]] 压扁为 Par[A]
    *
    * 使用函数 join 完成第二步
    */
  def join[A](ppa: Par[Par[A]]): Par[A] =
    es ⇒ {
      val pa = run(es)(ppa).get
      run(es)(pa)
    }

  /**
    * Exercise 7.14 实现 join 函数，并使用 map + join 实现 flatMap
    */
  def flatMapViaJoin[A, B](pa: Par[A])(f: A ⇒ Par[B]): Par[B] =
    join(map(pa)(f))

  def joinViaFlatMap[A](ppa: Par[Par[A]]): Par[A] =
    flatMap(ppa)(pa ⇒ pa)

  /**
    * UnitFuture 是一个包裹常量 get 的简单的 Future 实现，因此无法使用超时、取消等，只能简单返回 get 值
    */
  private case class UnitFuture[A](get: A) extends Future[A] {

    def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    def isCancelled: Boolean = false

    def isDone: Boolean = true

    def get(timeout: Long, unit: TimeUnit): A = get
  }

}
