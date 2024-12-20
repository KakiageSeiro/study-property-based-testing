import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.{be, noException}
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.should.Matchers.{shouldBe, shouldEqual}
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.Checkers

import scala.collection.SortedMap

class 料金2Spec extends AnyFunSuite with Matchers {

  test("正常に作成される") {
    val range = SortedMap(
      250 -> Money2(250),
      50 -> Money2(120),
      100 -> Money2(140),
      150 -> Money2(210),
    )

    // 例外が投げられてないことを検証
    noException should be thrownBy 料金List2(range, Money2(300))
  }

  // 料金List2は第２引数に、上限なしの料金を取るようになったためテストは不要になった
  // test("料金2が空の場合は例外が発生する") {
  //   val tiers = List.empty[料金2]
  //
  //   val exception = intercept[AssertionError] {
  //     料金List2(tiers)
  //   }
  //
  //   exception.getMessage should include ("料金2は必ず一つ以上")
  // }

  // 料金List2は第２引数に、上限なしかどうかを指定せず、料金だけを取るようになったためテストは不要になった
  // test("最後の料金2に上限値がないこと") {
  //   val tiers = List(
  //     料金2(Some(50), Money2(120)),
  //     料金2(Some(100), Money2(140)),
  //     料金2(Some(150), Money2(210)),
  //     料金2(Some(250), Money2(250)),
  //     料金2(Some(300), Money2(300)) // 最後に上限値あり
  //   )
  //
  //   val exception = intercept[AssertionError] {
  //     料金List2(tiers)
  //   }
  //
  //   exception.getMessage should include ("最後は上限値なし")
  // }

  // 料金List2は第一引数で上限値が必ずあるSortedMapを取るようになったのでテストは不要になった
  // test("上限値なしの料金2が複数存在する場合") {
  //   val tiers = List(
  //     料金2(Some(50), Money2(120)),
  //     料金2(None, Money2(140)),
  //     料金2(Some(150), Money2(210)),
  //     料金2(None, Money2(300))
  //   )
  //
  //   val exception = intercept[AssertionError] {
  //     料金List2(tiers)
  //   }
  //
  //   exception.getMessage should include ("上限値なしは一つだけ")
  // }

  // 料金List2は第一引数で上限値が必ずあるSortedMapを取るようになったのでテストは不要になった
  // test("上限値が重複している場合") {
  //   val tiers = List(
  //     料金2(Some(50), Money2(120)),
  //     料金2(Some(100), Money2(140)),
  //     料金2(Some(100), Money2(210)), // 重複
  //     料金2(Some(250), Money2(250)),
  //     料金2(None, Money2(300))
  //   )
  //
  //   val exception = intercept[AssertionError] {
  //     料金List2(tiers)
  //   }
  //
  //   exception.getMessage should include ("上限値は重複しない")
  // }

  // 料金List2は第一引数で上限値が必ずあるSortedMapを取るようになったのでテストは不要になった
  // test("上限値がソートされていない場合") {
  //   val tiers = List(
  //     料金2(Some(100), Money2(140)),
  //     料金2(Some(50), Money2(120)),  // 先頭のはずの料金2が2番目
  //     料金2(Some(150), Money2(210)),
  //     料金2(Some(250), Money2(250)),
  //     料金2(None, Money2(300))
  //   )
  //
  //   val exception = intercept[AssertionError] {
  //     料金List2(tiers)
  //   }
  //
  //   exception.getMessage should include ("上限値でソートされている")
  // }

  test("単一料金2（上限なし）の場合") {
    noException should be thrownBy 料金List2(SortedMap(), Money2(300))
  }

  // 料金List2は第一引数で上限値が必ずあるSortedMapを取るようになったのでテストは不要になった
  // test("上限値が正しく昇順に並んでいる場合") {
  //   val tiers = List(
  //     料金2(Some(10), Money2(100)),
  //     料金2(Some(20), Money2(200)),
  //     料金2(Some(30), Money2(300)),
  //     料金2(None, Money2(400))
  //   )
  //
  //   noException should be thrownBy 料金List2(tiers)
  // }

  test("上限値が同じでないがリスト内に重複する料金2が存在する場合") {
    val range = SortedMap(
      250 -> Money2(250),
      50 -> Money2(120),
      100 -> Money2(250),
      150 -> Money2(250),
    )

    noException should be thrownBy 料金List2(range, Money2(300))
  }
}

// -----------------------------------------------------------------------------------------------------------------------


// プロパティベースドテスト
class 料金2Spec2 extends AnyPropSpec with Checkers {
  // ジェネレータの定義
  val genMoney2: Gen[Money2] = Gen.choose(1, 10000).map(Money2(_))

  val gen料金2: Gen[料金2] = for {
    upperLimit <- Gen.choose(1, 9999)
    price      <- genMoney2
  } yield 料金2(上限値 = upperLimit, アカウントあたりの価格 = price)

  // 有効な料金List2のジェネレータ
  val genValid料金List2: Gen[料金List2] = for {
    size        <- Gen.choose(1, 10)
    upperLimits <- Gen.listOfN(size, Gen.choose(1, 9999)).map(_.sorted.distinct)
    prices      <- Gen.sequence[List[Money2], Money2](
      upperLimits.map(_ => genMoney2)
    )
    lastPrice   <- genMoney2
  } yield 料金List2(
    // upperLimits.zip(prices)はSeq[(Int, Money2)]を返すが､SortedMapのコンストラクタでSeqを直接取れない。ので*を付けることで(Int, Money2), (Int, Money2), (Int, Money2)のように展開する
    range = SortedMap(upperLimits.zip(prices)*),
    last = lastPrice
  )

  // プロパティの定義

  // SortedMapの時点でまちがいないんだけどね。ここは練習がてら正しく型によって制約を表現出来ているかどうかを確認するって事で
  property("rangeのキーはソートされており、一意である") {
    forAll(genValid料金List2) { 料金List2Instance =>
      val keys = 料金List2Instance.range.keys.toList
      keys ==  keys.sorted && keys.distinct ==  keys
    }
  }
}