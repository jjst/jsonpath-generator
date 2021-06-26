import org.json.JSONObject

object GeneratorApp extends App {
  println("hi")
}

case class JsonPathExpression(json: JSONObject, tokens: Seq[JsonPathToken]) {

}

sealed trait JsonPathToken

object Begin extends JsonPathToken
case class SelectField(fieldName: String) extends JsonPathToken
case class IndexArray(index: ArrayIndex)

sealed trait ArrayIndex
case class IndexValue(idx: Int) extends ArrayIndex
case class IndexRange(from: Int, to: Int) extends ArrayIndex
case object IndexWildCard extends ArrayIndex

object JsonPathExpression {
  def Empty(json: JSONObject): JsonPathExpression =
    JsonPathExpression(json, Seq.empty)
}