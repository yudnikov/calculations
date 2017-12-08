import ru.yudnikov.calculations._
import java.math.RoundingMode

import org.joda.money.CurrencyUnit
import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import ru.yudnikov.calculations.CurrencyRateProvider

import language.{implicitConversions, postfixOps}

object CalculationsExample extends App {

  implicit val calculationsContext: CalculationsContext = CalculationsContext(
    ExampleCurrencyRateProvider(),
    ExampleTaxRateProvider(),
    RoundingMode.HALF_UP
  )

  /*val y = (((20.RUR.~ + 20.RUR) to CurrencyUnit.USD) + 50.EUR) - 59.72.USD to CurrencyUnit.of("RUR")
  println(y.traceMonolith())
  y.trace foreach println

  println()

  val z = (100.RUR.~ to CurrencyUnit.USD) + 20.USD
  z.trace foreach println*/

  //val x = ((100.RUR.~ * 10.~ to CurrencyUnit.USD) ~+ 30.EUR) * factor
  //x.tracksPrint()

  //println(100.RUR.~ == 100.RUR.~)

  /*
  val a = 100.RUR ~* 3
  val moneyCalc = MoneyCalc(100.RUR)
  val factorCalc = FactorCalc(3)
  val b = MoneyCalc(ScalaMoney(300.RUR), Operation.*, moneyCalc :: factorCalc :: Nil)
  println(a == b)
  */

  val r = 100.RUR ~+ 50.RUR
  val a = MoneyCalc(100.RUR)
  val b = MoneyCalc(50.RUR)
  println(r == MoneyCalc(ScalaMoney(150.RUR), Operation.+, a :: b :: Nil))

}
