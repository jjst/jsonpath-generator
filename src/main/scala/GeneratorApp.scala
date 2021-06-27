import io.circe.Json

object GeneratorApp extends App {
  val json = Json.Null
  println(JsonPathExpression.Empty(json).text)
  println(JsonPathExpression.Empty(json).nextValidTokens)
}

