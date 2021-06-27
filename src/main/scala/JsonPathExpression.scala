import JsonPathToken.{IndexArray, SelectField}
import io.circe.{Json, JsonObject}

case class InvalidJsonPathException(err: String) extends RuntimeException(err)

class JsonPathExpression(val json: Json, val tokens: List[JsonPathToken]) {
  private def fieldSelectors(o: JsonObject): Set[JsonPathToken] =
    o.keys.map(SelectField.apply).toSet

  private def indexSelectors(array: Vector[Json]): Set[JsonPathToken] = {
    if (array.isEmpty) {
      Set.empty
    } else {
      val positiveIndices = array.indices.map(i => ArrayIndex.Selection(Seq(i)))
      (positiveIndices ++ Seq(ArrayIndex.Selection(Seq(-1)), ArrayIndex.Wildcard))
        .map(IndexArray.apply)
        .toSet
    }
  }

  lazy val nextValidTokens: Set[JsonPathToken] = {
    if (json.isNull) {
      Set.empty
    }
    else if (tokens.isEmpty) {
      Set(JsonPathToken.Begin)
    } else {
      json.fold(
        Set.empty,
        _ => Set.empty,
        _ => Set.empty,
        _ => Set.empty,
        indexSelectors,
        fieldSelectors
      )
    }
  }

  def add(token: JsonPathToken): JsonPathExpression = {
    if (!nextValidTokens.contains(token)) {
      throw InvalidJsonPathException(s"${token} cannot be added to the current jsonpath expression")
    } else {
      val tokens = token :: this.tokens
      new JsonPathExpression(select(json, token), tokens)
    }
  }

 private def select(json: Json, token: JsonPathToken): Json = {
   token match {
     case JsonPathToken.Begin => json
     case _ => ???
   }
 }

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
