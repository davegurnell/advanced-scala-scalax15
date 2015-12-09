import scala.concurrent.{Await,Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits._

import scalaz.Monoid
import scalaz.syntax.monoid._

object MapReduce {
  def foldMap[A, B](data: List[A])(func: A => B)(implicit monoid: Monoid[B]): B = {
    data.foldLeft(mzero[B])(_ |+| func(_))
  }

  def parallelFoldMap[A, B](data: List[A])(func: A => B)(implicit monoid: Monoid[B]): Future[B] = {
    val numCpus = Runtime.getRuntime.availableProcessors
    val groupSize = math.ceil(data.length.toDouble / numCpus).toInt

    val groups: List[List[A]] = data.grouped(groupSize).toList

    val futureGroups: List[Future[List[A]]] = groups.map(Future(_))

    val futureResults: List[Future[B]] =
      futureGroups.map { futureGroup: Future[List[A]] =>
        futureGroup.map { group: List[A] =>
          foldMap(group)(func)
        }
      }

    val resultsFuture: Future[List[B]] =
      Future.sequence(futureResults)

    val finalFutire: Future[B] =
      resultsFuture.map(results => foldMap(results)(identity))

    finalFutire
  }
}

object Main extends App {
  import scalaz.std.anyVal._

  val data: List[String] =
    (1 to 1000).map(_.toString).toList

  val result: Future[Int] =
    MapReduce.parallelFoldMap(data)(_.toInt)

  println(Await.result(result, 2.seconds))
}