import scala.language.higherKinds

import scala.concurrent.{Await,Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits._

import scalaz.\/
import scalaz.Applicative
import scalaz.Monoid
import scalaz.syntax.functor._
import scalaz.syntax.monoid._
import scalaz.std.list._

object MapReduce {
  def foldMap[A, B](data: List[A])(func: A => B)(implicit monoid: Monoid[B]): B = {
    data.foldLeft(mzero[B])(_ |+| func(_))
  }

  def parallelFoldMap[F[_], A, B](data: List[A])(func: A => B)(implicit monoid: Monoid[B], applicative: Applicative[F]): F[B] = {
    val numCpus = Runtime.getRuntime.availableProcessors
    val groupSize = math.ceil(data.length.toDouble / numCpus).toInt

    val groups: List[List[A]] = data.grouped(groupSize).toList

    val fGroups: List[F[List[A]]] = groups.map(applicative.point(_))

    val fResults: List[F[B]] =
      fGroups.map { fGroup: F[List[A]] =>
        fGroup.map { group: List[A] =>
          foldMap(group)(func)
        }
      }

    val resultsF: F[List[B]] =
      applicative.sequence(fResults)

    val finalF: F[B] =
      resultsF.map(results => foldMap(results)(identity))

    finalF
  }
}

object Main extends App {
  import scalaz.std.anyVal._
  import scalaz.std.scalaFuture._
  import scalaz.std.option._

  val data: List[String] =
    (1 to 1000).map(_.toString).toList

  val futureResult: Future[Int] =
    MapReduce.parallelFoldMap[Future, String, Int](data)(_.toInt)

  val optionResult: Option[Int] =
    MapReduce.parallelFoldMap[Option, String, Int](data)(_.toInt)

  type Result[A] = String \/ A

  val eitherResult: Result[Int] =
    MapReduce.parallelFoldMap[Result, String, Int](data)(_.toInt)

  println(Await.result(futureResult, 2.seconds))
  println(optionResult)
  println(eitherResult)
}