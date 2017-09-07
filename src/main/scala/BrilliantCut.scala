import cats.data.NonEmptyList
import cats.implicits._
import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.{Errors, JsonObject}

object BrilliantCut {

  def largestProfit(gems: Seq[Gem]): Int =
    gems.map(calculateMaxProfitsForRawChunks).map(_.sum).sum

  def largestProfit(json: String): Either[Errors, Int] = {
    decode[JsonObject](json) match {
      case Right(jsonObject) =>
        jsonObject.values
          .map(_.as[Gem])
          .map(_.toValidated)
          .map(_.bimap(List(_), List(_)))
          .combineAll
          .bimap(errors => Errors(NonEmptyList.fromListUnsafe(errors)),
                 gems => largestProfit(gems))
          .toEither
      case Left(error) =>
        Left(Errors(NonEmptyList.of(error)))
    }
  }

  private def calculateMaxProfitsForRawChunks(gem: Gem): Seq[Int] = {
    val memoized = new Memoize(calculateMaxProfitForRawChunk(gem.cuts))
    gem.rawChunks.map(memoized)
  }

  private def calculateMaxProfitForRawChunk(availableCuts: Seq[Cut])(
      rawChunk: Int): Int = {
    generateCombinationsOfCuts(rawChunk, availableCuts.toStream, Seq.empty)
      .map(calculateProfitForCombinationOfCuts(rawChunk))
      .max
  }

  private def calculateProfitForCombinationOfCuts(rawChunk: Int)(
      actualCuts: Seq[Cut]): Int = {
    val value = actualCuts.map(_.value).sum
    val sumOfCutSizes = actualCuts.map(_.size).sum
    val waste = rawChunk - sumOfCutSizes
    val profit = value - waste
    profit
  }

  private def generateCombinationsOfCuts(
      chunkSize: Int,
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
