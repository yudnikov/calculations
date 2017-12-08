import java.math.RoundingMode
import org.scalatest.{FlatSpec, Matchers}
import ru.yudnikov.calculations._

class ConversionsSuit extends FlatSpec with Matchers {

  implicit val calculationsContext: CalculationsContext = CalculationsContext(
    ExampleCurrencyRateProvider(),
    ExampleTaxRateProvider(),
    RoundingMode.HALF_UP
  )

  "Currency conversions" should "be associative" in {
    val x = 100.RUR + 20.USD - 2.USD - 18.USD
    x.money shouldEqual 100.RUR
  }

}
