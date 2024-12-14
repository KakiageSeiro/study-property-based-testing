import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

// いままでは振る舞い駆動スタイルのAnyFlatSpecをつかってたけど、もっとシンプルっぽいAnyFunSuiteを使ってみる
class TierSpec extends AnyFunSuite with Matchers {

  // ヘルパー関数で料金を作成
  def createTier(upperLimit: Option[Int], price: Int): Tier = Tier(upperLimit, Money(price))

  test("有効な料金リストが正常に作成される") {
    val tiers = List(
      createTier(Some(50), 120),
      createTier(Some(100), 140),
      createTier(Some(150), 210),
      createTier(Some(250), 250),
      createTier(None, 300)
    )

    noException should be thrownBy Tiers(tiers)
  }

  test("ティアが空の場合は例外が発生する") {
    val tiers = List.empty[Tier]

    val exception = intercept[AssertionError] {
      Tiers(tiers)
    }

    exception.getMessage should include ("ティアは必ず一つ以上")
  }

  test("最後のティアに上限値がないこと") {
    val tiers = List(
      createTier(Some(50), 120),
      createTier(Some(100), 140),
      createTier(Some(150), 210),
      createTier(Some(250), 250),
      createTier(Some(300), 300) // 最後に上限値あり
    )

    val exception = intercept[AssertionError] {
      Tiers(tiers)
    }

    exception.getMessage should include ("最後は上限値なし")
  }

  test("上限値なしのティアが複数存在する場合") {
    val tiers = List(
      createTier(Some(50), 120),
      createTier(None, 140),
      createTier(Some(150), 210),
      createTier(None, 300)
    )

    val exception = intercept[AssertionError] {
      Tiers(tiers)
    }

    exception.getMessage should include ("上限値なしは一つだけ")
  }

  test("上限値が重複している場合") {
    val tiers = List(
      createTier(Some(50), 120),
      createTier(Some(100), 140),
      createTier(Some(100), 210), // 重複
      createTier(Some(250), 250),
      createTier(None, 300)
    )

    val exception = intercept[AssertionError] {
      Tiers(tiers)
    }

    exception.getMessage should include ("上限値は重複しない")
  }

  test("上限値がソートされていない場合") {
    val tiers = List(
      createTier(Some(100), 140),
      createTier(Some(50), 120),  // ソート順が逆
      createTier(Some(150), 210),
      createTier(Some(250), 250),
      createTier(None, 300)
    )

    val exception = intercept[AssertionError] {
      Tiers(tiers)
    }

    exception.getMessage should include ("上限値でソートされている")
  }

  test("単一ティア（上限なし）の場合") {
    val tiers = List(
      createTier(None, 300)
    )

    noException should be thrownBy Tiers(tiers)
  }

  test("上限値が正しく昇順に並んでいる場合") {
    val tiers = List(
      createTier(Some(10), 100),
      createTier(Some(20), 200),
      createTier(Some(30), 300),
      createTier(None, 400)
    )

    noException should be thrownBy Tiers(tiers)
  }

  test("上限値が同じでないがリスト内に重複する料金が存在する場合") {
    val tiers = List(
      createTier(Some(50), 120),
      createTier(Some(100), 140),
      createTier(Some(150), 210),
      createTier(Some(250), 250),
      createTier(None, 300),
      createTier(Some(250), 250) // 重複しているが上限値は既に重複
    )

    val exception = intercept[AssertionError] {
      Tiers(tiers)
    }

    exception.getMessage should include ("上限値は重複しない")
  }
}
