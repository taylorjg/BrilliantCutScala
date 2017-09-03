import scala.collection.mutable

class Memoize[I, O](f: I => O) extends mutable.HashMap[I, O] {
  override def apply(key: I): O = getOrElseUpdate(key, f(key))
}
