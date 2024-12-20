import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*

import scala.collection.SortedMap

// IronType[A, C] という書き方でIronを使える。Aは基本型、Cは制約
// IronType[Int, Greater[0]] で0より大きいIntを表す
// エイリアスとして Int :| Greater[0] と書ける
// Ironを使った型は、使っていない型のサブタイプになる。つまり以下ができる
// val x: Int :| Greater[0] = ???
// val y: Int = x // これができるよ

opaque type Money3 = Int :| Greater[0]
object Money3 {
  def apply(value: Int :| Greater[0]): Money3 = value
  def value(m: Money3): Int = m

  // 実行時の値はこれで制約を満たしているかチェックする
  // いままでコンストラクタでアサーションして制約を表現していたもののEither版
  def refine(value: Int): Either[String, Money3] =
    value.refineEither[Greater[0]]

  // 例外を投げるリファインメント（信頼できると分かっている値を扱いたい時用）
  def unsafeRefine(value: Int): Money3 =
    refine(value).fold(error => throw new IllegalArgumentException(error), identity)
}

case class 料金3(上限値: Int, アカウントあたりの価格: Money3)

case class 料金List3(range: SortedMap[Int, Money3], last: Money3) {
  def 上限値List = range.keys
}

