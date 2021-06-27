import io.circe.Json

object JsonPathGenerator {
  def stream(json: Json): LazyList[JsonPathExpression] = {
    val expr = JsonPathExpression.Empty(json)
    stream(expr)
  }

  private def stream(expr: JsonPathExpression): LazyList[JsonPathExpression] = {
    val nextExpressions = expr.nextValidTokens.to(LazyList).map(expr.add)
    nextExpressions.flatMap { e =>
      e #:: stream(e)
    }
  }
}
