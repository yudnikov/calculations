import org.joda.time.DateTime

import ru.yudnikov.calculations._

case class ExampleTaxRateProvider() extends TaxRateProvider{
  override val dateTime: DateTime = new DateTime(2017, 12, 4, 12, 0)
  override def getRate(taxCode: TaxCode): BigDecimal = taxCode match {
    case TaxCode.VAT => 18
    case _ => 1
  }
}
