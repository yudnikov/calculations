import ru.yudnikov.calculations._
import java.math.RoundingMode
import ru.yudnikov.calculations.CurrencyRateProvider
import language.{implicitConversions, postfixOps}

object CalculationsExample extends App {

  implicit val currencyRateProvider: CurrencyRateProvider = ExampleRateProvider
  implicit val roundingMode: RoundingMode = RoundingMode.HALF_UP

  val x = 100.RUR + 20.USD - 2.USD - 18.USD

}