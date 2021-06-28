import io.circe.Json
import io.circe._
import io.circe.parser._

import scala.io.Source


object GeneratorApp extends App {
  val json = Json.Null
  val filePath = args(1)
  val jsonStr = Source.fromFile(filePath).mkString
  parse(jsonStr) match {
    case Left(failure) => println(failure)
    case Right(json) => {
      val expressions = JsonPathExpression.generateAll(json)
      println(expressions.toList.map(_.text).size)
    }
  }
}

