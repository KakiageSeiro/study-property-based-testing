// [型とデータ構造で制約を表現する](https://gakuzzzz.github.io/slides/describe_constraints_by_types_and_data_structures/#19)
// の料金表の例を使う
//
// 範囲/アカウントあたりの価格
// 50 人未満 / 120円
// 100人未満 / 140円
// 150人未満 / 210円
// 250人未満 / 250円
// 上限なし  / 300円


opaque type Money = Int
object Money {
  def apply(value: Int): Money = value
  def value(m: Money): Int = m
}

case class 料金(上限値: Option[Int], アカウントあたりの価格: Money)

case class 料金List(values: List[料金]) {
  def 上限値List = values.flatMap(_.上限値)

  assert(values.nonEmpty,                    "ティアは必ず一つ以上")
  assert(values.last.上限値.isEmpty,          "最後は上限値なし")
  assert(values.count(_.上限値.isEmpty) == 1, "上限値なしは一つだけ")
  assert(上限値List.distinct == 上限値List,    "上限値は重複しない")
  assert(上限値List.sorted == 上限値List,      "上限値でソートされている")
}







