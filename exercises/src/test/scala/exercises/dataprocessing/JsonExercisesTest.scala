package exercises.dataprocessing

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import exercises.dataprocessing.JsonExercises._

class JsonExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {
  val john: Json = JsonObject(
    Map(
      "name"   -> JsonString(" John Doe "),
      "age"    -> JsonNumber(25),
      "isDead" -> JsonBoolean(false),
      "address" -> JsonObject(
        Map(
          "street-number" -> JsonNumber(25),
          "street-name"   -> JsonString("  Cody Road")
        )
      )
    )
  )

  test("trimAll") {
    assert(
      trimAll(john) == JsonObject(
        Map(
          "name"   -> JsonString("John Doe"),
          "age"    -> JsonNumber(25),
          "isDead" -> JsonBoolean(false),
          "address" -> JsonObject(
            Map(
              "street-number" -> JsonNumber(25),
              "street-name"   -> JsonString("Cody Road")
            )
          )
        )
      )
    )
  }

  test("anonymize") {
    assert(
      anonymize(john) == JsonObject(
        Map(
          "name"   -> JsonString("***"),
          "age"    -> JsonNumber(0),
          "isDead" -> JsonBoolean(false),
          "address" -> JsonObject(
            Map(
              "street-number" -> JsonNumber(0),
              "street-name"   -> JsonString("***")
            )
          )
        )
      )
    )
  }

  test("search") {
    assert(search(JsonObject(Map.empty), "ll", maxDepth = 99) == false)
    assert(search(JsonNumber(5), "ll", maxDepth = 99) == false)
    assert(search(JsonBoolean(false), "ll", maxDepth = 99) == false)
    assert(search(JsonString("Hello"), "ll", maxDepth = 99) == true)
    assert(search(JsonObject(Map("message" -> JsonString("Hello"))), "ll", maxDepth = 99) == true)
    assert(search(JsonObject(Map("message" -> JsonString("Hello"))), "ss", maxDepth = 99) == false)
    assert(search(JsonObject(Map("message" -> JsonString("hi"))), "ll", maxDepth = 99) == false)
  }

  test("depth") {
    assert(depth(JsonNumber(1)) == 0)
    assert(depth(JsonBoolean(false)) == 0)
    assert(depth(JsonObject(Map.empty)) == 0)
    assert(depth(JsonObject(Map("k" -> JsonNumber(1)))) == 1)
    assert(depth(john) == 2)
  }

}
