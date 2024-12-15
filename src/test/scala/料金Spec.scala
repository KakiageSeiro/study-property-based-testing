import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.{be, noException}
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.should.Matchers.shouldBe
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.Checkers

import scala.util.{Random, Try}

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


// プロパティベースドテスト
// 狙ったテストデータを作るジェネレータを定義→forAllの内部で検証する条件を指定
// という流れで書く
class 料金Spec2 extends AnyPropSpec with Checkers {
  // ジェネレータ(Gen.chooseで生成する範囲を決めると、そこからランダムで選ばれる)
  val genMoney: Gen[Money] = Gen.choose(1, 10000).map(Money(_))
  val gen料金: Gen[料金] = for {
    upperLimit <- Gen.option(Gen.choose(1, 9999))
    price      <- genMoney
  } yield 料金(upperLimit, price)

  // 有効な料金リストのジェネレータ
  val genValid料金List: Gen[List[料金]] = for {
    size <- Gen.choose(1, 10)
    upperLimits <- Gen.listOfN(size, Gen.choose(1, 9999)).map(_.sorted.distinct)
    somePrices <- Gen.sequence[List[料金], 料金](
      upperLimits.map { limit =>
        for {
          price <- genMoney
        } yield 料金(Some(limit), price)
      }
    )
    lastPrice <- gen料金.map(_.copy(上限値 = None))
  } yield somePrices :+ lastPrice

  // 無効な料金リストのジェネレータ（各制約を一つずつ破る）
  // ここで型パラメータに指定している(String, List[料金])はforAll(ジェネレータから生成した値を検証する部分)を呼び出すときに受け取れる
  // Gen.oneOfは引数の中からランダムにジェネレータを一つとる
  val genInvalid料金List: Gen[(String, List[料金])] = Gen.oneOf(
    // 空リスト
    Gen.const(("空リスト", List.empty)),

    // 最後の要素に上限値がある
    for {
      size  <- Gen.choose(1, 10)
      prices <- Gen.listOfN(size, for {
        upper <- Gen.option(Gen.choose(1, 9999))
        price <- genMoney
      } yield 料金(upper, price))
      if prices.nonEmpty && prices.last.上限値.isDefined
    } yield ("最後に上限値がある", prices),

    // 上限値なしの料金が複数存在
    for {
      size      <- Gen.choose(2, 10)
      somePrices <- Gen.listOfN(size - 2, for {
        upper <- Gen.option(Gen.choose(1, 9999))
        price <- genMoney
      } yield 料金(upper, price))
      nonePrice1 <- gen料金.map(_.copy(上限値 = None))
      nonePrice2 <- gen料金.map(_.copy(上限値 = None))
      prices     = somePrices :+ nonePrice1 :+ nonePrice2
    } yield ("上限値なしが複数", prices),

    // 上限値が重複している
    for {
      size       <- Gen.choose(2, 10)
      duplicate  <- Gen.choose(1, 9999)
      somePrices <- Gen.listOfN(size, for {
        upper <- Gen.const(Some(duplicate))
        price <- genMoney
      } yield 料金(upper, price))
      lastPrice  <- gen料金.map(_.copy(上限値 = None))
      prices     = somePrices :+ lastPrice
    } yield ("上限値が重複", prices),

    // 上限値がソートされていない
    for {
      size        <- Gen.choose(2, 10)
      upperLimits <- Gen.listOfN(size, Gen.choose(1, 9999))
      shuffled    <- Gen.const(util.Random.shuffle(upperLimits))
      if shuffled != shuffled.sorted
      somePrices  <- Gen.sequence[List[料金], 料金](
        shuffled.map { limit =>
          genMoney.map(price => 料金(Some(limit), price))
        }
      )
      lastPrice   <- gen料金.map(_.copy(上限値 = None))
      prices      = somePrices :+ lastPrice
    } yield ("上限値がソートされていない", prices)
  )

  // -----------------------------------------------------------------------------------------------------------------------
  // プロパティはテスト対象のあるべき姿みたいな。たとえば、料金は"上限値でソートされている"というプロパティを持っているのような感じだと思う
  // forAllはProp(プロパティ)を返す。第２引数で

  property("有効な料金リストは正常に構築されるべき") {
    forAll(genValid料金List) { prices =>
      // forAllの中でboolean(result.isSuccess)を書きたいがために無理やりTryしている感があり、なんとかしたい。コンストラクタでアサーションしているからこうなっちゃうのはしょうがないか？
      val result = scala.util.Try(料金List(prices))
      result.isSuccess
    }
  }

  // 無効な料金リストはAssertionErrorを投げるべき
  property("Invalid 料金List should throw AssertionError") {
    forAll(genInvalid料金List) { case (description, prices) =>
      val result = scala.util.Try(料金List(prices))
      result.isFailure && result.failed.get.isInstanceOf[AssertionError]
    }
  }
}