// [型とデータ構造で制約を表現する](https://gakuzzzz.github.io/slides/describe_constraints_by_types_and_data_structures/#19)
// の料金表の例を使う
//
// 範囲/アカウントあたりの価格
// 50 人未満 / 120円
// 100人未満 / 140円
// 150人未満 / 210円
// 250人未満 / 250円
// 上限なし  / 300円

// opaque type Money : Int

case class Money(value: Int)
case class Tier(upperLimit: Option[Int], pricePerAccount: Money) {
  def isEmpty: Boolean = true
}

case class Tiers(values: List[Tier]) {
  def upperLimits = values.flatMap(_.upperLimit)

  assert(values.nonEmpty,              "ティアは必ず一つ以上")
  assert(values.last.upperLimit.isEmpty,    "最後は上限値なし")
  assert(values.count(_.isEmpty) == 1, "上限値なしは一つだけ")
  assert(upperLimits.distinct == upperLimits, "上限値は重複しない")
  assert(upperLimits.sorted == upperLimits,   "上限値でソートされている")
}







