package futures

import scala.concurrent.{ExecutionContext, Future}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */

  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */

  def transform[A](l: List[Either[Throwable, A]]): (List[A], List[Throwable]) = {
    l.foldLeft(List.empty[A], List.empty[Throwable])((a, b) => {
      b match {
        case Right(value) => (a._1 :+ value, a._2)
        case Left(error) => (a._1, a._2 :+ error)
      }
    })
  }

  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {

    val listFutures = futures.map {
      item =>
        item
          .map(Right(_))
          .recover {
            case error: Throwable =>
              Left(error)
          }
    }


    val result = Future.sequence(listFutures).map {
      item => transform(item)
    }

    result
  }
}
