import scala.collection.SortedMap

// IronType[A, C] という書き方でIronを使える。Aは基本型、Cは制約
// IronType[Int, Greater[0]] で0より大きいIntを表す
// エイリアスとして Int :| Greater[0] と書ける
// Ironを使った型は、使っていない型のサブタイプになる。つまり以下ができる
// val x: Int :| Greater[0] = ???
// val y: Int = x // これができるよ

opaque type Money3 = Int
object Money3 {
  def apply(value: Int): Money3 = value
  def value(m: Money3): Int = m
}

case class 料金3(上限値: Option[Int], アカウントあたりの価格: Money3)

case class 料金List3(range: SortedMap[Int, Money3], last: Money3) {
  def 上限値List = range.keys
}

