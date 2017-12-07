import ru.yudnikov.calculations._
import java.math.RoundingMode

import org.joda.money.CurrencyUnit
import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import ru.yudnikov.calculations.CurrencyRateProvider

import language.{implicitConversions, postfixOps}

object CalculationsExample extends App {

  implicit val currencyRateProvider: CurrencyRateProvider = ExampleRateProvider()
  implicit val roundingMode: RoundingMode = RoundingMode.HALF_UP

  val y = (((20.RUR.~ + 20.RUR) to CurrencyUnit.USD) + 50.EUR) - 59.72.USD to CurrencyUnit.of("RUR")
  println(y.trace())
  y.tracePrint().sortBy(_._2).foreach(t => println(s"${t._2}) ${t._1}"))

  println()

  val z = (100.RUR.~ to CurrencyUnit.USD) + 20.USD
  z.tracePrint().sortBy(_._2).foreach(t => println(s"${t._2}) ${t._1}"))

}