import io.circe.Json
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import org.scalatest.prop.TableDrivenPropertyChecks._

import JsonPathToken._


class JsonPathExpressionSpec extends AnyWordSpec with Matchers {
  val jsons = Table(
    "json",
    Json.False,
    Json.True,
    Json.arr(Json.fromInt(1)),
    Json.obj("test" -> Json.fromInt(1)),
    Json.fromInt(1),
    Json.fromFloatOrNull(1.0f)
  )
  "JsonPathExpression.nextValidTokens" when {
    "the current json is null" should {
      "return the empty set" in {
        val exp = JsonPathExpression.Empty(Json.Null)
        exp.nextValidTokens shouldBe Set.empty
      }
    }
    "the current json is not null and the jsonpath expression is empty" should {
      "return the anchor" in {
        forAll(jsons) { json =>
          val exp = JsonPathExpression.Empty(json)
          exp.nextValidTokens shouldBe Set(JsonPathToken.Begin)
        }
      }
    }
    "the current json is an object" should {
      "return valid index selectors" in {
        val jsonObject = Json.arr(
          Json.fromInt(1),
          Json.fromString("hi"),
          Json.obj("f" -> Json.fromInt(1)),
        )
        val exp = new JsonPathExpression(jsonObject, tokens = List(Begin))
        exp.nextValidTokens should contain theSameElementsAs Set(
          IndexArray(ArrayIndex.Wildcard),
          IndexArray(ArrayIndex.Selection(Seq(0))),
          IndexArray(ArrayIndex.Selection(Seq(1))),
          IndexArray(ArrayIndex.Selection(Seq(2))),
          IndexArray(ArrayIndex.Selection(Seq(-1))),
        )
      }
      "return valid field selectors" in {
      }
    }
  }

  "JsonPathExpression.add" when {
    "adding an invalid token" should {
      "throw" in {
        val exp = JsonPathExpression.Empty(Json.True)
        a [InvalidJsonPathException] shouldBe thrownBy {
          // This would actually be a valid jsonpath if we supported
          // unanchored jsonpath, but we don't right now 🤷
          exp.add(JsonPathToken.SelectField("field"))
        }
      }
    }
    "adding the begin anchor token" should {
      "return the anchored jsonpath expression" in {
        forAll(jsons) { json =>
          val exp = JsonPathExpression.Empty(json)
          val newExp = exp.add(Begin)
          newExp.tokens shouldBe List(Begin)
          newExp.json shouldBe json
        }
      }
    }
  }
}
