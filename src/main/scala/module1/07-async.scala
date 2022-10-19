package module1

import module1.threads.async
import module1.utils.NameableThreads

import java.io.File
import java.util.{Timer, TimerTask}
import java.util.concurrent.{Callable, Executor, ExecutorService, Executors, ForkJoinPool, ThreadFactory, ThreadPoolExecutor}
import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future, Promise, TimeoutException}
import scala.io.{BufferedSource, Source}
import scala.language.{existentials, postfixOps}
import scala.util.{Failure, Success, Try, Using}

object threads {

  class Thread1 extends Thread {
    override def run(): Unit = {
      println(s"Hello from " +
        s"${Thread.currentThread().getName}")
    }
  }


  def printRunningTime(v: => Unit): Unit = {
    val start = System.currentTimeMillis()
    v
    val end = System.currentTimeMillis()
    println(s"Execution time ${end - start}")
  }

  def getRatesLocation1 = async {
    Thread.sleep(1000)
    println("GetRatesLocation1")
  }

  def getRatesLocation2 = async {
    Thread.sleep(2000)
    println("GetRatesLocation2")
  }

  def async(f: => Unit): Thread = new Thread {
    override def run(): Unit = f
  }

  def async2[A](f: => A): A = {
    var v: A = null.asInstanceOf[A]
    val t = new Thread {
      override def run(): Unit = {
        v = f
      }
    }
    t.start()
    t.join
    v
  }

  def getRatesLocation3 = async2 {
    Thread.sleep(1000)
    println(s"Hello from " +
      s"${Thread.currentThread().getName}")
    println("GetRatesLocation1")
    10
  }

  def getRatesLocation4 = async2 {
    Thread.sleep(2000)
    println(s"Hello from " +
      s"${Thread.currentThread().getName}")
    println("GetRatesLocation2")
    20
  }


  class ToyFuture[T] private(v: () => T) {
    private var r: T = null.asInstanceOf[T]
    private var isCompleted: Boolean = false
    private val q = mutable.Queue[T => _]()


    def map[B](f: T => B): ToyFuture[B] = ???

    def flatMap[B](f: T => ToyFuture[B]): ToyFuture[B] = ???

    def onComplete[U](f: T => U): Unit = {
      if (isCompleted) f(r)
      else q.enqueue(f)
    }

    private def start(executor: Executor) = {
      val t = new Runnable {
        override def run(): Unit = {
          val result = v()
          r = result
          isCompleted = true
          while (q.nonEmpty) {
            q.dequeue()(result)
          }
        }
      }
      executor.execute(t)
    }

  }

  object ToyFuture {
    def apply[T](v: => T)(implicit executor: Executor): ToyFuture[T] = {
      val f = new ToyFuture[T](() => v)
      f.start(executor)
      f
    }
  }

  implicit val ex: Executor = executor.pool1

  def getRatesLocation5 = ToyFuture {
    Thread.sleep(1000)
    println(s"Hello from " +
      s"${Thread.currentThread().getName}")
    println("GetRatesLocation1")
    10
  }

  def getRatesLocation6 = ToyFuture {
    Thread.sleep(2000)
    println(s"Hello from " +
      s"${Thread.currentThread().getName}")
    println("GetRatesLocation2")
    20
  }


  import scala.concurrent.ExecutionContext.Implicits.global

  val f1: Future[String] = Future("Hello from scala future")
  val f2 = Future.successful("Hello from scala future")
  val f3: Future[Nothing] = Future.failed(new Throwable())

  val f4 = Future.fromTry(Try())
  val r1: String = Await.result(f1, 5 seconds)
  val r2: Future[String] = Await.ready(f1, 5 seconds)

}


object executor {
  val pool1: ExecutorService = Executors.newFixedThreadPool(2, NameableThreads("fixed-pool-1"))
  val pool2: ExecutorService = Executors.newCachedThreadPool(NameableThreads("cached-pool-2"))
  val pool3: ExecutorService = Executors.newWorkStealingPool(4)
  val pool4: ExecutorService = Executors.newSingleThreadExecutor(NameableThreads("singleThread-pool-4"))
}

/**
 * Try:
 * - безопасно выполнить вычисление, котоое потенциально может завершиться с ошибкой
 * - отделить логику обработки ошибки от вычисления
 *
 * ADT - sum
 * sealed abstract class Try[+T]
 * final case class Failure[+T](exception: Throwable) extends Try[T]
 * final case class Success[+T](value: T) extends Try[T]
 */


object tryObj {


  def readFromFile(): List[String] = {
    val s: BufferedSource =
      Source.fromFile(new File("ints.txt"))

    //если exception здесь - то ресурс не закроетсяя
    /*
        val result = s.getLines().toList
        s.close()
        result
    */
    //Никакой трансформации
    val result = try {
      s.getLines().toList
    } catch {
      case e =>
        println(e.getMessage)
        Nil
    } finally {
      s.close()
    }
    result
  }

//Как же принести композицию? Возвращаем Try
  def readFromFile2(): Try[List[Int]] = {
    val source: Try[BufferedSource] =
      Try(Source.fromFile(new File("ints.txt")))

    def lines(s: Source) = Try(s.getLines().toList.map(_.toInt))

    val r: Try[List[Int]] = for {
      s <- source
      l <- lines(s)
    } yield l
    source.foreach(_.close())
    r
  }

}


/**
 * Future - это контейнер для значения, которое появляется в будующем
 * - конструкторы
 * - основные комбинаторы
 * - Execution Context
 */
object future {

  // глобальный контекст
  //import scala.concurrent.ExecutionContext.Implicits.global
  /**
   *
   */
  //  def getRatesLocation1 = Future{
  //    Thread.sleep(1000)
  //    println("GetRatesLocation1")
  //    10
  //  }
  //
  //  def getRatesLocation2 = Future{
  //    Thread.sleep(2000)
  //    println("GetRatesLocation2")
  //    20
  //  }
  //
  //  def printRunningTime[T](v: => Future[T]): Future[T] = {
  //
  //    for{
  //      start <- Future.successful(System.currentTimeMillis())
  //      r <- v
  //      end <- Future.successful(System.currentTimeMillis())
  //      _ <- Future.successful(println(s"Execution time ${end - start}"))
  //    } yield r
  //  }
  //

  /**
   * Этот вызов не блокирует main поток
   * всегда возвращает Future
   * Если failure то map не отработает
   */
  //  val r1: Future[Int] = getRatesLocation1.map{ i =>
  //    i + 10
  //  }
  //
  /**
   * Здесь i + 10 не имеет смысла, foreach предназначен для side effect
   */
  //  getRatesLocation1.foreach{ i =>
  //    println(i)
  //  }
  //
  /**
   * Принимает функцию Try => T
   * тоже side effect - те Future должна быть закончена
   */
  //  getRatesLocation1 onComplete{
  //    case Success(value) => println(value)
  //    case Failure(exception) => println(exception.getMessage)
  //  }
  //
  /**
   * ^ ^
   * Те если нужен side effect то foreeach или onComplete
   * Если хотим изменить что то внутри future то map
   */


  /**
   * Как еще обработать ошибку помимо onComplete не потеряв значение?
   * Используем метод recover
    */
  //  val r2: Future[Int] = getRatesLocation1.recover{
  //    case e => 0
  //  }


  /**
   * Как создавать executionContext
   * 1. Using Java
   */
  val ec = ExecutionContext.fromExecutor(executor.pool1)
  val ec2 = ExecutionContext.fromExecutor(executor.pool2)
  val ec3 = ExecutionContext.fromExecutor(executor.pool3)
  val ec4 = ExecutionContext.fromExecutor(executor.pool4)

  def action(v: Int): Int = {
    Thread.sleep(1000)
    println(s"Action $v in ${Thread.currentThread().getName}")
    v
  }


  val f1 = Future(action(10))(ec)
  val f2 = Future(action(20))(ec2)

  /**
   * Оборачиваем во future
   * f1 и f2 на своих ec
   * И еще внешне на ec3
   * и вложенный action на ec4
   * Action in cache
   * Каждый выполнится на своем thread pool
   */  val f3 = f1.flatMap { v1 =>
    action(50)
    f2.map { v2 =>
      action(v1 + v2)
    }(ec4)
  }(ec3)

}


/**
 * Promises
 */
object promise {

  val p1: Promise[Int] = Promise[Int]
  val f1: Future[Int] = p1.future


  /**
   * применение promises
   */
  object FutureSyntax {

    implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

    def map[T, B](future: Future[T])(f: T => B): Future[B] = {
      val p = Promise[B]
      future.onComplete {
        case Failure(exception) => p.failure(exception)
        case Success(value) => p.success(f(value))
      }
      p.future
    }

    def flatMap[T, B](future: Future[T])(f: T => Future[B]): Future[B] = ???


    def make[T](v: => T)(implicit ec: ExecutionContext): Future[T] = {
      val p = Promise[T]
      val r = new Runnable {
        override def run(): Unit = {
          p.complete(Try(v))
        }
      }
      ec.execute(r)
      p.future
    }

    def make[T](v: => T, timeout: Long): Future[T] = {
      val p = Promise[T]
      val timer = new Timer(true)
      val task = new TimerTask {
        override def run(): Unit = ???
      }
      timer.schedule(task, timeout)
      ???
    }
  }
}
