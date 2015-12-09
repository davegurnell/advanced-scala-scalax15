import scalaz.\/
import scalaz.Kleisli
import scalaz.Validation
import scalaz.std.list._
import scalaz.syntax.std.string._
import scalaz.syntax.either._
import scalaz.syntax.validation._
import scalaz.syntax.applicative._
import scalaz.syntax.kleisli._

object valexample {
  type Result[A] = List[String] \/ A
  type Rule[A, B] = Kleisli[Result, A, B]

  object Rule {
    def apply[A, B](func: A => Result[B]): Rule[A, B] =
      Kleisli[Result, A, B](func)
  }

  case class Address(house: Int, street: String)
  case class Person(name: String, address: Address)

  def numeric(msg: String = "Must be a number") =
    Rule[String, Int] { value =>
      value.parseInt.leftMap(exn => List(msg)).disjunction
    }

  def positive(msg: String = "Must be positive") =
    Rule[Int, Int] { number =>
      if(number > 0) number.right else List(msg).left
    }

  def nonEmpty(msg: String = "Must not be empty") =
    Rule[String, String] { value =>
      val trimmed = value.trim
      if(trimmed.isEmpty) List(msg).left else trimmed.right
    }

  def getValue(key: String, msg: String = "Key not found") =
    Rule[Map[String, String], String] { data =>
      data.get(key).fold(List(msg).left[String])(_.right)
    }

  def houseNumberOk =
    getValue("house", "House number not found") andThen
    numeric("House number must be numeric") andThen
    positive("House number must be positive")

  def streetOk =
    getValue("street", "Street number not found") andThen
    nonEmpty("Street name must ne non-blank")

  def addressOk =
    Rule[Map[String, String], Address] { data =>
      (
        houseNumberOk(data).validation |@|
        streetOk(data).validation
      )(Address.apply).disjunction
    }

  val data = Map[String, String](
    "house"  -> "-29",
    "street" -> "",
    "name"   -> "Bananaman"
  )

  def main(args: Array[String]): Unit = {
    println(addressOk(data))
  }
}