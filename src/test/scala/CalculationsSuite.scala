import java.math.RoundingMode

import org.scalatest.{FlatSpec, Matchers}
import ru.yudnikov.calculations.{ScalaMoney, _}

class CalculationsSuite extends FlatSpec with Matchers {

  implicit val calculationsContext: CalculationsContext = CalculationsContext(
    ExampleCurrencyRateProvider(),
    ExampleTaxRateProvider(),
    RoundingMode.HALF_UP
  )

  "Money * Double" should "give MoneyCalc" in {
    100.RUR ~* 3 shouldEqual {
      val moneyCalc = MoneyCalc(100.RUR)
      val factorCalc = FactorCalc(3)
      MoneyCalc(ScalaMoney(300.RUR), Operation.*, moneyCalc :: factorCalc :: Nil)
    }
    3 ~* 100.RUR shouldEqual {
      val moneyCalc = MoneyCalc(100.RUR)
      val factorCalc = FactorCalc(3)
      MoneyCalc(ScalaMoney(300.RUR), Operation.*, factorCalc :: moneyCalc :: Nil)
    }
    100.RUR ~* 3 shouldEqual 3 ~* 100.RUR
  }

  "Money + Money" should "give MoneyCalc and be associative" in {
    100.RUR ~+ 50.RUR shouldEqual {
      val a = MoneyCalc(100.RUR)
      val b = MoneyCalc(50.RUR)
      MoneyCalc(ScalaMoney(150.RUR), Operation.+, a :: b :: Nil)
    }
    50.RUR ~+ 100.RUR shouldEqual {
      val a = MoneyCalc(50.RUR)
      val b = MoneyCalc(100.RUR)
      MoneyCalc(ScalaMoney(150.RUR), Operation.+, a :: b :: Nil)
    }
  }

}
