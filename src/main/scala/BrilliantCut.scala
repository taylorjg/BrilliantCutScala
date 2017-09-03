object BrilliantCut {

  def largestProfit(input: Input): Int = {
    val gems = Seq(input.diamond, input.sapphire, input.ruby)
    gems.map(calculateMaxProfitsForRawChunks).map(_.sum).sum
  }

  private def memoize[I, O](f: I => O): I => O =
    new scala.collection.mutable.HashMap[I, O]() {
      override def apply(key: I): O = getOrElseUpdate(key, f(key))
    }

  private def calculateMaxProfitsForRawChunks(gem: Gem): Seq[Int] = {
    val memoized = memoize(calculateAllProfitsForRawChunk(gem.cuts))
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

  private def generateCombinationsOfCuts(
      chunkSize: Int,
      availableCuts: Stream[Cut],
      actualCuts: Seq[Cut]): Stream[Seq[Cut]] = {
    availableCuts.flatMap(availableCut => {
      val remainingChunkSize = chunkSize - availableCut.size
      if (remainingChunkSize > 0) {
        val newActualCuts = actualCuts :+ availableCut
        Stream.cons(newActualCuts, generateCombinationsOfCuts(remainingChunkSize, availableCuts, newActualCuts))
      }
      else {
        Stream.empty
      }
    })
  }
}
