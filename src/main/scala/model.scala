case class Input(diamond: Gem, sapphire: Gem, ruby: Gem)

case class Gem(cuts: Seq[Cut], rawChunks: Seq[Int])

case class Cut(size: Int, value: Int)
