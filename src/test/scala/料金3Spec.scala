import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.{be, noException}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.Checkers

import scala.collection.SortedMap

class 料金3Spec extends AnyFunSuite with Matchers {

  test("正常に作成される") {
    val range = SortedMap(
      350 -> Money3.unsafeRefine(350),
      50 -> Money3.unsafeRefine(130),
      100 -> Money3.unsafeRefine(140),
      150 -> Money3.unsafeRefine(310),
    )

    // 例外が投げられてないことを検証
    noException should be thrownBy 料金List3(range, Money3.unsafeRefine(300))
  }

  test("単一料金3（上限なし）の場合") {
    noException should be thrownBy 料金List3(SortedMap(), Money3.unsafeRefine(300))
  }

  test("上限値が同じでないがリスト内に重複する料金3が存在する場合") {
    val range = SortedMap(
      350 -> Money3.unsafeRefine(350),
      50 -> Money3.unsafeRefine(130),
      100 -> Money3.unsafeRefine(350),
      150 -> Money3.unsafeRefine(350),
    )

    noException should be thrownBy 料金List3(range, Money3.unsafeRefine(300))
  }

  test("applyに直接篩型を渡す") {
    // assumeで制約を満たすことが分かっているものはassumeでキャストできる
    val refined: Int :| Greater[0] = 300.assume
    noException should be thrownBy Money3(refined)
  }

  test("refineに1を渡すと作成に成功する") {
    Money3.refine(1) shouldBe a[Right[_, _]]
  }

  test("refineで0以下の値では作成に失敗する") {
    val invalidValue = 0
    Money3.refine(invalidValue) shouldBe a[Left[_, _]]
  }

  test("unsafeRefineに1を渡すと作成に成功する") {
    noException should be thrownBy Money3.unsafeRefine(1)
  }

  test("unsafeRefineで0以下の値では例外が投げられる") {
    intercept[IllegalArgumentException] {
      Money3.unsafeRefine(0)
    }
  }
}

// -----------------------------------------------------------------------------------------------------------------------


// プロパティベースドテスト
class 料金3Spec_propertyBased extends AnyPropSpec with Checkers {
  // ジェネレータの定義
  // 篩型に変更し、unsafeRefineで値を生成しているため、例えばGen.choose(-10, 10000)のように制約を満たさない値を含めても、エラーが投げられた値はジェネレータがMoneyを生成しなくなるため、テストとしてはエラーにならない
  // つまり、篩型にした時点でプロパティベースドテストに頼らずとも、堅牢に実装できていると言える
  val genMoney3: Gen[Money3] = Gen.choose(1, 10000).map(Money3.unsafeRefine)

  val gen料金3: Gen[料金3] = for {
    upperLimit <- Gen.choose(1, 9999)
    price <- genMoney3
  } yield 料金3(上限値 = upperLimit, アカウントあたりの価格 = price)

  // 有効な料金List3のジェネレータ
  val genValid料金List3: Gen[料金List3] = for {
    size <- Gen.choose(1, 10)
    upperLimits <- Gen.listOfN(size, Gen.choose(1, 9999)).map(_.sorted.distinct)
    prices <- Gen.sequence[List[Money3], Money3](
      upperLimits.map(_ => genMoney3)
    )
    lastPrice <- genMoney3
  } yield 料金List3(
    // upperLimits.zip(prices)はSeq[(Int, Money3)]を返すが､SortedMapのコンストラクタでSeqを直接取れない。ので*を付けることで(Int, Money3), (Int, Money3), (Int, Money3)のように展開する
    range = SortedMap(upperLimits.zip(prices) *),
    last = lastPrice
  )

  // プロパティの定義

  // SortedMapの時点でまちがいないんだけどね。ここは練習がてら正しく型によって制約を表現出来ているかどうかを確認するって事で
  property("rangeのキーはソートされており、一意である") {
    forAll(genValid料金List3) { 料金List3Instance =>
      val keys = 料金List3Instance.range.keys.toList
      keys == keys.sorted && keys.distinct == keys
    }
  }
}