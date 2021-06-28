import JsonPathToken.{IndexArray, SelectField}
import io.circe
import io.circe.{Json, JsonNumber, JsonObject}

case class InvalidJsonPathException(err: String) extends RuntimeException(err)

sealed trait JsonPathSelection {
  def selected: Seq[Json]

  def transform(f: Json => Json): JsonPathSelection

  def flatMap(f: Json => JsonPathSelection): JsonPathSelection

  def isEmpty: Boolean
}

object JsonPathSelection {
  def apply(nodes: Seq[Json]): JsonPathSelection = nodes match {
    case Nil => JsonPathSelection.Empty
    case singleNode :: Nil => JsonPathSelection.Single(singleNode)
    case more => JsonPathSelection.Multiple(more)
  }

  object Empty extends JsonPathSelection {
    override def selected: Seq[Json] = Seq.empty

    override def transform(f: Json => Json): JsonPathSelection = Empty

    override def flatMap(f: Json => JsonPathSelection): JsonPathSelection = Empty

    override def isEmpty: Boolean = true
  }
  case class Single(node: Json) extends JsonPathSelection {
    val selected = Seq(node)

    override def transform(f: Json => Json): JsonPathSelection =
      JsonPathSelection.Single(f(node))

    override def flatMap(f: Json => JsonPathSelection): JsonPathSelection = f(node)

    override def isEmpty: Boolean = false
  }
  case class Multiple(nodes: Seq[Json]) extends JsonPathSelection {
    val selected = nodes

    override def transform(f: Json => Json): JsonPathSelection =
      JsonPathSelection.Multiple(selected.map(f))

    override def flatMap(f: Json => JsonPathSelection): JsonPathSelection = {
      JsonPathSelection(nodes.flatMap(node => f(node).selected))
    }

    override def isEmpty: Boolean = false
  }
}

//FIXME: replace Json with `JsonPathSelection`, to address failing test.
case class JsonPathExpression(currentSelection: JsonPathSelection, tokens: List[JsonPathToken]) {
  private def fieldSelectors(o: JsonObject): Set[JsonPathToken] = {
    o.keys.map(SelectField.apply).toSet
  }

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
    tokens match {
      case Nil =>
        Set(JsonPathToken.Begin)
      case _ => {
        currentSelection.selected.map { json =>
          json.fold(
            Set.empty,
            _ => Set.empty,
            _ => Set.empty,
            _ => Set.empty,
            indexSelectors,
            fieldSelectors,
          )
        }.flatten.toSet
      }
    }
  }

  def add(token: JsonPathToken): JsonPathExpression = {
    if (!nextValidTokens.contains(token)) {
      throw InvalidJsonPathException(s"${token} cannot be added to the current jsonpath expression")
    } else {
      val tokens = token :: this.tokens
      new JsonPathExpression(select(currentSelection, token), tokens)
    }
  }

 private def select(selection: JsonPathSelection, token: JsonPathToken): JsonPathSelection = {
   token match {
     case JsonPathToken.Begin =>
       selection
     case JsonPathToken.SelectField(field) => {
       selection.flatMap { json =>
         json.asObject.fold[JsonPathSelection](JsonPathSelection.Empty)(o =>
           o(field).fold[JsonPathSelection](JsonPathSelection.Empty)(JsonPathSelection.Single.apply)
         )
       }
     }
     case JsonPathToken.IndexArray(index) => {
       selection.flatMap { json =>
         json.asArray.fold[JsonPathSelection](JsonPathSelection.Empty)(array => {
           val filtered = index match {
             case ArrayIndex.Selection(indices) =>
               indices.map { i =>
                 // Support negative indices
                 if (i >= 0) array(i) else array(array.length + i)
               }
             case ArrayIndex.Range(from, to) =>
               (from to to).map(array.apply)
             case ArrayIndex.Wildcard =>
               array
           }
           JsonPathSelection(filtered)
         })
       }
     }
   }
 }

  def text: String = {
    tokens.reverse.map(_.text).mkString("")
  }
}

object JsonPathExpression {
  def Empty(json: Json): JsonPathExpression =
    new JsonPathExpression(JsonPathSelection.Single(json), List.empty)

  def generateAll(json: Json): LazyList[JsonPathExpression] = {
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

sealed trait JsonPathToken {
  def text: String
  def isIndexSelector: Boolean
  def isFieldSelector: Boolean
}

object JsonPathToken {
  object Begin extends JsonPathToken {
    val text = "$"
    override def isIndexSelector: Boolean = false
    override def isFieldSelector: Boolean = false
  }

  case class SelectField(fieldName: String) extends JsonPathToken {
    val text = s".$fieldName"
    override def isIndexSelector: Boolean = false
    override def isFieldSelector: Boolean = true
  }

  case class IndexArray(index: ArrayIndex) extends JsonPathToken {
    override def isIndexSelector: Boolean = true
    override def isFieldSelector: Boolean = false
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
