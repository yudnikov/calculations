import org.joda.money.CurrencyUnit
import org.joda.time.DateTime
import ru.yudnikov.calculations.CurrencyRateProvider

case class ExampleCurrencyRateProvider() extends CurrencyRateProvider {
  import CurrencyUnit._
  override val dateTime: DateTime = new DateTime(2017, 12, 4, 12, 0)
  private val RUR = CurrencyUnit.of("RUR")
  override def getRate(numerator: CurrencyUnit, denominator: CurrencyUnit): BigDecimal = {
    numerator -> denominator match {
      case (USD, EUR) => 0.846904986
      case (EUR, USD) => 1.180769999
      case (USD, RUR) => 59.1715976
      case (RUR, USD) => 0.0169
      case (EUR, RUR) => 69.8680473
      case (RUR, EUR) => 0.0143126943
      case _ => 1
    }
  }
}