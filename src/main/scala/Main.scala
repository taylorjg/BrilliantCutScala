import io.circe.generic.auto._
import io.circe.parser.decode

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val json = Source.fromResource("input.json").getLines.mkString
    decode[Input](json) match {
      case Right(input) =>
         println(s"largestProfit: ${BrilliantCut.largestProfit(input)}")
      case Left(error) =>
        println(s"Failed to read input.json: $error")
    }
  }
}
