import org.scalatest.{FreeSpec, MustMatchers}
//import io.circe.generic.auto._
//import io.circe.parser.decode

import scala.io.Source

class testSpec extends FreeSpec with MustMatchers {

  "BrilliantCut tests" - {

    "given example" - {
      "largest profit" in {
        test("input1.json", 27)
      }
    }

    "given example x 2 raw chunks" - {
      "largest profit" in {
        test("input2.json", 27 * 2)
      }
    }

    "given example x 2 gem types" - {
      "largest profit" in {
        test("input3.json", 27 * 2)
      }
    }

    "given example x 2 gem types x 2 raw chunks" - {
      "largest profit" in {
        test("input4.json", 27 * 2 * 2)
      }
    }
  }

  private def test(resource: String, expected: Int): Unit = {
    val json = Source.fromResource(resource).getLines.mkString
    BrilliantCut.largestProfit(json) match {
      case Right(actual) =>
        actual mustBe expected
      case Left(error) =>
        fail(s"Failed to decode contents of $resource: $error")
    }
  }
}
