import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val json = Source.fromResource("input.json").getLines.mkString
    timeIt(BrilliantCut.largestProfit(json)) match {
      case (Right(largestProfit), elapsedTime) =>
        println(s"largestProfit: $largestProfit; elapsed time: ${elapsedTime}ms")
      case (Left(errors), elapsedTime) =>
        println(s"FAILED: ${errors.toList}; elapsed time: ${elapsedTime}ms")
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
