import scalaz.\/
import scalaz.Validation
import scalaz.std.list._
import scalaz.syntax.std.string._
import scalaz.syntax.either._
import scalaz.syntax.validation._
import scalaz.syntax.applicative._
import scalaz.syntax.kleisli._

object valexample {
  type Result[A] = List[String] \/ A
  type Rule[A, B] = A => Result[B]

  case class Address(house: Int, street: String)
  case class Person(name: String, address: Address)

  def numeric(msg: String = "Must be a number"): Rule[String, Int] =
    (value: String) =>
      value.parseInt.leftMap(exn => List(msg)).disjunction

  def positive(msg: String = "Must be positive"): Rule[Int, Int] =
    (number: Int) =>
      if(number > 0) number.right else List(msg).left

  def nonEmpty(msg: String = "Must not be empty"): Rule[String, String] =
    (value: String) => {
    val trimmed = value.trim
    if(trimmed.isEmpty) List(msg).left else trimmed.right
  }

  def getValue(key: String, msg: String = "Key not found"): Rule[Map[String, String], String] =
    (data: Map[String, String]) =>
      data.get(key).fold(List(msg).left[String])(_.right)

  def houseNumberOk(data: Map[String, String]): Result[Int] = for {
    string <- getValue("house", "House number not found")(data)
    number <- numeric("House number must be numeric")(string)
    posNum <- positive("House number must be positive")(number)
  } yield posNum

  def streetOk(data: Map[String, String]): Result[String] = for {
    string   <- getValue("street", "Street number not found")(data)
    neString <- nonEmpty("Street name must ne non-blank")(string)
  } yield neString

  def addressOk(data: Map[String, String]): Result[Address] = (
    houseNumberOk(data).validation |@|
    streetOk(data).validation
  )(Address.apply).disjunction

  val data = Map[String, String](
    "house"  -> "-29",
    "street" -> "",
    "name"   -> "Bananaman"
  )

  def main(args: Array[String]): Unit = {
    println(addressOk(data))
  }
}