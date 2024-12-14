import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

// いままでは振る舞い駆動スタイルのAnyFlatSpecをつかってたけど、もっとシンプルっぽいAnyFunSuiteを使ってみる
class 料金Spec extends AnyFunSuite with Matchers {

  // ヘルパー関数で料金を作成
  def create料金(上限値: Option[Int], price: Int): 料金 = 料金(上限値, Money(price))

  test("有効な料金リストが正常に作成される") {
    val tiers = List(
      create料金(Some(50), 120),
      create料金(Some(100), 140),
      create料金(Some(150), 210),
      create料金(Some(250), 250),
      create料金(None, 300)
    )

    noException should be thrownBy 料金List(tiers)
  }

  test("ティアが空の場合は例外が発生する") {
    val tiers = List.empty[料金]

    val exception = intercept[AssertionError] {
      料金List(tiers)
    }

    exception.getMessage should include ("ティアは必ず一つ以上")
  }

  test("最後のティアに上限値がないこと") {
    val tiers = List(
      create料金(Some(50), 120),
      create料金(Some(100), 140),
      create料金(Some(150), 210),
      create料金(Some(250), 250),
      create料金(Some(300), 300) // 最後に上限値あり
    )

    val exception = intercept[AssertionError] {
      料金List(tiers)
    }

    exception.getMessage should include ("最後は上限値なし")
  }

  test("上限値なしのティアが複数存在する場合") {
    val tiers = List(
      create料金(Some(50), 120),
      create料金(None, 140),
      create料金(Some(150), 210),
      create料金(None, 300)
    )

    val exception = intercept[AssertionError] {
      料金List(tiers)
    }

    exception.getMessage should include ("上限値なしは一つだけ")
  }

  test("上限値が重複している場合") {
    val tiers = List(
      create料金(Some(50), 120),
      create料金(Some(100), 140),
      create料金(Some(100), 210), // 重複
      create料金(Some(250), 250),
      create料金(None, 300)
    )

    val exception = intercept[AssertionError] {
      料金List(tiers)
    }

    exception.getMessage should include ("上限値は重複しない")
  }

  test("上限値がソートされていない場合") {
    val tiers = List(
      create料金(Some(100), 140),
      create料金(Some(50), 120),  // ソート順が逆
      create料金(Some(150), 210),
      create料金(Some(250), 250),
      create料金(None, 300)
    )

    val exception = intercept[AssertionError] {
      料金List(tiers)
    }

    exception.getMessage should include ("上限値でソートされている")
  }

  test("単一ティア（上限なし）の場合") {
    val tiers = List(
      create料金(None, 300)
    )

    noException should be thrownBy 料金List(tiers)
  }

  test("上限値が正しく昇順に並んでいる場合") {
    val tiers = List(
      create料金(Some(10), 100),
      create料金(Some(20), 200),
      create料金(Some(30), 300),
      create料金(None, 400)
    )

    noException should be thrownBy 料金List(tiers)
  }

  test("上限値が同じでないがリスト内に重複する料金が存在する場合") {
    val tiers = List(
      create料金(Some(50), 120),
      create料金(Some(100), 140),
      create料金(Some(150), 210),
      create料金(Some(250), 250),
      create料金(None, 300),
      create料金(Some(250), 250) // 重複しているが上限値は既に重複
    )

    val exception = intercept[AssertionError] {
      料金List(tiers)
    }

    exception.getMessage should include ("上限値は重複しない")
  }
}
