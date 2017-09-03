import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.Error

object BrilliantCut {

  def largestProfit(json: String): Either[Error, Int] = {
    decode[Input](json).map(largestProfit)
  }

  def largestProfit(input: Input): Int = {
    val gems = Seq(input.diamond, input.sapphire, input.ruby)
    gems.map(calculateMaxProfitsForRawChunks).map(_.sum).sum
  }

  private def calculateMaxProfitsForRawChunks(gem: Gem): Seq[Int] = {
    val memoized = new Memoize(calculateAllProfitsForRawChunk(gem.cuts))
    gem.rawChunks.map(memoized).map(_.max)
  }

  private def calculateAllProfitsForRawChunk(availableCuts: Seq[Cut])(
      rawChunk: Int): Seq[Int] = {
    generateCombinationsOfCuts(rawChunk, availableCuts.toStream, Seq.empty).distinct
      .map(calculateProfitForCombinationOfCuts(rawChunk))
  }

  private def calculateProfitForCombinationOfCuts(rawChunk: Int)(
      actualCuts: Seq[Cut]): Int = {
    val value = actualCuts.map(_.value).sum
    val sumOfCutSizes = actualCuts.map(_.size).sum
    val waste = rawChunk - sumOfCutSizes
    val profit = value - waste
    profit
  }

  private def generateCombinationsOfCuts(chunkSize: Int,
                                         availableCuts: Stream[Cut],
                                         actualCuts: Seq[Cut]): Stream[Seq[Cut]] =
    availableCuts.flatMap(availableCut => {
      val remainingChunkSize = chunkSize - availableCut.size
      if (remainingChunkSize > 0) {
        def sortCutsBySize(a: Cut, b: Cut) = a.size == b.size
        val newActualCuts =
          (availableCut +: actualCuts).sortWith(sortCutsBySize)
        newActualCuts #::
          generateCombinationsOfCuts(remainingChunkSize,
                                     availableCuts,
                                     newActualCuts)
      } else {
        Stream.empty
      }
    })
  }
