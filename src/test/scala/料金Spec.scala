import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

// いままでは振る舞い駆動スタイルのAnyFlatSpecをつかってたけど、もっとシンプルっぽいAnyFunSuiteを使ってみる
class 料金Spec extends AnyFunSuite with Matchers {

  test("正常に作成される") {
    val tiers = List(
      料金(Some(50), Money(120)),
      料金(Some(100), Money(140)),
      料金(Some(150), Money(210)),
      料金(Some(250), Money(250)),
      料金(None, Money(300))
    )

    // 例外が投げられてないことを検証
    noException should be thrownBy 料金List(tiers)
  }

  test("料金が空の場合は例外が発生する") {
    val tiers = List.empty[料金]

    val exception = intercept[AssertionError] {
      料金List(tiers)
    }

    exception.getMessage should include ("料金は必ず一つ以上")
  }

  test("最後の料金に上限値がないこと") {
    val tiers = List(
      料金(Some(50), Money(120)),
      料金(Some(100), Money(140)),
      料金(Some(150), Money(210)),
      料金(Some(250), Money(250)),
      料金(Some(300), Money(300)) // 最後に上限値あり
    )

    val exception = intercept[AssertionError] {
      料金List(tiers)
    }

    exception.getMessage should include ("最後は上限値なし")
  }

  test("上限値なしの料金が複数存在する場合") {
    val tiers = List(
      料金(Some(50), Money(120)),
      料金(None, Money(140)),
      料金(Some(150), Money(210)),
      料金(None, Money(300))
    )

    val exception = intercept[AssertionError] {
      料金List(tiers)
    }

    exception.getMessage should include ("上限値なしは一つだけ")
  }

  test("上限値が重複している場合") {
    val tiers = List(
      料金(Some(50), Money(120)),
      料金(Some(100), Money(140)),
      料金(Some(100), Money(210)), // 重複
      料金(Some(250), Money(250)),
      料金(None, Money(300))
    )

    val exception = intercept[AssertionError] {
      料金List(tiers)
    }

    exception.getMessage should include ("上限値は重複しない")
  }

  test("上限値がソートされていない場合") {
    val tiers = List(
      料金(Some(100), Money(140)),
      料金(Some(50), Money(120)),  // 先頭のはずの料金が2番目
      料金(Some(150), Money(210)),
      料金(Some(250), Money(250)),
      料金(None, Money(300))
    )

    val exception = intercept[AssertionError] {
      料金List(tiers)
    }

    exception.getMessage should include ("上限値でソートされている")
  }

  test("単一料金（上限なし）の場合") {
    val tiers = List(
      料金(None, Money(300))
    )

    noException should be thrownBy 料金List(tiers)
  }

  test("上限値が正しく昇順に並んでいる場合") {
    val tiers = List(
      料金(Some(10), Money(100)),
      料金(Some(20), Money(200)),
      料金(Some(30), Money(300)),
      料金(None, Money(400))
    )

    noException should be thrownBy 料金List(tiers)
  }

  test("上限値が同じでないがリスト内に重複する料金が存在する場合") {
    val tiers = List(
      料金(Some(50), Money(120)),
      料金(Some(100), Money(140)),
      料金(Some(150), Money(210)),
      料金(Some(250), Money(250)),
      料金(Some(251), Money(250)), // 上限値が同じでないがリスト内に重複する料金が存在
      料金(None, Money(300))
    )

    noException should be thrownBy 料金List(tiers)
  }
}
