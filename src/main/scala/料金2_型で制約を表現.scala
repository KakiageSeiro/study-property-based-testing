import scala.collection.SortedMap

opaque type Money2 = Int
object Money2 {
  def apply(value: Int): Money2 = value
  def value(m: Money2): Int = m
}

case class 料金2(上限値: Option[Int], アカウントあたりの価格: Money2)

case class 料金List2(range: SortedMap[Int, Money2], last: Money2) {
  def 上限値List = range.keys
}

