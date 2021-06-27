import io.circe.Json
import org.json.JSONObject

object GeneratorApp extends App {
  val json = Json.Null
  println(JsonPathExpression.Empty(json).text)
  println(JsonPathExpression.Empty(json).nextValidTokens)
}

class JsonPathExpression(json: Json, tokens: List[JsonPathToken]) {
  lazy val nextValidTokens: Set[JsonPathToken] = {
    if (json.isNull) {
      Set.empty
    }
    else if (tokens.isEmpty) {
      Set(JsonPathToken.Begin)
    } else {
      ???
    }
  }

  def add(token: JsonPathToken): JsonPathExpression =
    ???

  def text: String = {
    tokens.map(_.text).mkString("")
  }
}

object JsonPathExpression {
  def Empty(json: Json): JsonPathExpression =
    new JsonPathExpression(json, List.empty)
}

sealed trait JsonPathToken {
  def text: String
}

object JsonPathToken {
  object Begin extends JsonPathToken {
    val text = "$"
  }

  case class SelectField(fieldName: String) extends JsonPathToken {
    val text = s".$fieldName"
  }

  case class IndexArray(index: ArrayIndex) extends JsonPathToken {
    val text = {
      val idxText = index match {
        case ArrayIndex.Selection(idx) =>
          idx.mkString(",")
        case ArrayIndex.Range(from, to) =>
          s"$from:$to"
        case ArrayIndex.Wildcard =>
          "*"
      }
      s"[$idxText]"
    }
  }
}

sealed trait ArrayIndex
object ArrayIndex {
  // Can be negative (last element), and should be a list of values (el selection)
  case class Selection(idx: Seq[Int]) extends ArrayIndex

  case class Range(from: Int, to: Int) extends ArrayIndex

  case object Wildcard extends ArrayIndex
}
