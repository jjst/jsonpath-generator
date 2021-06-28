import io.circe.Json
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.prop.TableDrivenPropertyChecks._
import JsonPathToken._
import com.jayway.jsonpath.JsonPath
import io.circe.parser.parse
import org.scalatest.AppendedClues.convertToClueful

import scala.io.Source


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
      "return a set with the anchor token only" in {
        val exp = JsonPathExpression.Empty(Json.Null)
        exp.nextValidTokens shouldBe Set(JsonPathToken.Begin)
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
        val exp = new JsonPathExpression(JsonPathSelection.Single(jsonObject), tokens = List(Begin))
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
        val exp = new JsonPathExpression(JsonPathSelection.Single(jsonObject), tokens = List(Begin))
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
          newExp.currentSelection shouldBe JsonPathSelection.Single(json)
        }
      }
    }
  }

  "JsonPathExpress.generateAll" should {
    val expressions = Table(
      "json"                        -> "jsonpath expressions",
      """{ "field1": 1}"""          -> Seq("$", "$.field1"),
      """[{}]"""                    -> Seq("$", "$[0]", "$[-1]", "$[*]"),
      """[{ "field1": 1}]"""        -> Seq("$", "$[0]", "$[-1]", "$[*]", "$[0].field1", "$[-1].field1", "$[*].field1"),
      """[{ "f1": 1}, {"f2": 2}]""" -> Seq("$", "$[0]", "$[1]", "$[-1]", "$[*]",
                                           "$[0].f1", "$[1].f2", "$[-1].f2", "$[*].f1", "$[*].f2"),
      """[[1,2],[]]"""              -> Seq("$", "$[0]", "$[1]", "$[-1]", "$[*]",
                                          "$[0][0]", "$[0][1]","$[0][-1]","$[0][*]",
                                          "$[*][0]", "$[*][1]","$[*][-1]","$[*][*]",
                                      ),
      // Weird but valid
      """[{ "f1": 1}, [2]]""" -> Seq("$", "$[0]", "$[1]", "$[-1]", "$[*]",
                                     "$[0].f1",
                                     "$[1][0]", "$[1][-1]", "$[1][*]",
                                     "$[-1][0]", "$[-1][-1]", "$[-1][*]",
                                     "$[*][0]", "$[*][-1]", "$[*][*]",
                                     "$[*].f1"
      )
    )

    "generate all possible supported jsonpath expressions" in {
      import io.circe.parser._
      forAll(expressions) { case (jsonString, expected) =>
        val json = parse(jsonString).getOrElse(fail("Invalid json"))
        val allJsonPaths = JsonPathExpression.generateAll(json)
        val jsonpaths = allJsonPaths.map(_.text).toList
        jsonpaths should contain theSameElementsAs expected withClue(s"${jsonpaths.toSet.diff(expected.toSet)} - ${expected.toSet.diff(jsonpaths.toSet)}")
      }
    }

    "generate valid jsonpath expressions" in {
      val testFiles = Table(
        "Test file",
        "auto-generated.json",
        "cheriefm.json",
        "skyrock.json"
      )
      forAll(testFiles) { testFile =>
        val jsonString = Source.fromResource(s"json/${testFile}").mkString
        import org.scalatest.Inspectors._
        val json = parse(jsonString).getOrElse(fail("Invalid json"))
        val allJsonPaths = JsonPathExpression.generateAll(json)
        // All generate jsonpath expressions should be valid
        forAll(allJsonPaths) { jsonPathExpr =>
          val result: AnyRef = JsonPath.read(jsonString, jsonPathExpr.text)
        }
      }
    }
  }
}
