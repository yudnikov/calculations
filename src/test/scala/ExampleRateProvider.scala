import org.joda.money.CurrencyUnit
import ru.yudnikov.calculations.CurrencyRateProvider

object ExampleRateProvider extends CurrencyRateProvider {
  import CurrencyUnit._
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