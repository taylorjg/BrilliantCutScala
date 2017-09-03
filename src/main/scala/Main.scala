import io.circe.generic.auto._
import io.circe.parser.decode

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val json = Source.fromResource("input.json").getLines.mkString
    decode[Input](json) match {
      case Right(input) =>
        timeIt(BrilliantCut.largestProfit(input)) match {
          case (largestProfit, elapsedTime ) =>
            println(s"largestProfit: $largestProfit; elapsed time: ${elapsedTime}ms")
        }
      case Left(error) =>
        println(s"Failed to read input.json: $error")
    }
  }

  private def timeIt[R](block: => R): (R, Long) = {
    val start = System.nanoTime()
    val result = block
    val end = System.nanoTime()
    val ns = end - start
    val ms = ns / (1000 * 1000)
    (result, ms)
  }
}
