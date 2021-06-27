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
    "the current json is an array" should {
      "return valid index selectors" in {
        val jsonObject = Json.arr(
          Json.fromInt(1),
          Json.fromString("hi"),
          Json.obj("field" -> Json.fromInt(1)),
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
    }
    "the current json is an object" should {
      "return valid field selectors" in {
        val jsonObject = Json.obj(
          "field1" -> Json.fromInt(1),
        "field2" -> Json.fromInt(1)
        )
        val exp = new JsonPathExpression(jsonObject, tokens = List(Begin))
        exp.nextValidTokens should contain theSameElementsAs Set(
          SelectField("field1"),
          SelectField("field2")
        )
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

  "JsonPathExpress.generateAll" should {
    "generate all possible supported jsonpath expressions for object" in {
      val json = Json.obj(
        "field1" -> Json.fromInt(1)
      )
      val allJsonPath = JsonPathExpression.generateAll(json)
      allJsonPath.map(_.text).toList should contain theSameElementsAs Seq(
        "$",
        "$.field1"
      )
    }

    "generate all possible supported jsonpath expressions" in {
      val json = Json.arr(
        Json.obj("field1" -> Json.fromInt(1))
      )
      val allJsonPath = JsonPathExpression.generateAll(json)
      allJsonPath.map(_.text).toList should contain theSameElementsAs Seq(
        "$",
        "$[0]",
        "$[-1]",
        "$[*]",
        "$[0].field1",
        "$[-1].field1",
        "$[*].field1",
      )
    }
  }
}
