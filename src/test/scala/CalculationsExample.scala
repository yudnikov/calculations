import ru.yudnikov.calculations._
import java.math.RoundingMode

import org.joda.money.CurrencyUnit
import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import ru.yudnikov.calculations.CurrencyRateProvider

import language.{implicitConversions, postfixOps}
import scala.util.Random

import ru.yudnikov.utils._

object CalculationsExample extends App {

  implicit val calculationsContext: CalculationsContext = CalculationsContext(
    ExampleCurrencyRateProvider(),
    ExampleTaxRateProvider(),
    RoundingMode.HALF_UP
  )

  val x = ((100.RUR ~* 3.~% to CurrencyUnit.USD) ~+ 5.EUR to CurrencyUnit.EUR) ~+ 30.RUR *+ TaxCode.VAT
  x.tracksPrint()

  val y = ((2 ~+ 2) ~/ 5) + ((12 ~/ 2 ~* 3) ~- 2)
  y.tracksPrint()

  val z = (100.RUR ~/ 10 ~+ 2.RUR) * TaxCode.VAT
  z.tracksPrint()

  val r = new Random(new DateTime().getMillis)

  val moneys = 1 to 100 map { _ =>
    r.nextInt(100000).RUR
  }

  val taxes = moneys map { money =>
    val withVAT = money ~*+ TaxCode.VAT
    withVAT -> withVAT * 3.~%
  }

  println(taxes)
  trait Model[T] {
    val id: T
  }
  trait Slice
  case class Subdivision(id: String) extends Model[String] with Slice
  case class Employee(id: Long, name: String) extends Model[Long] with Slice
  val salesPerYear = 1200000.RUR
  val subdivisionToPercent = Map(
    Subdivision("Sales") -> 61.~%,
    Subdivision("Supply") -> 24.~%,
    Subdivision("IT") -> 9.~%,
  )
  val employeeToSubdivision = Map(
    Employee(1, "Oleg") -> Subdivision("Sales"),
    Employee(2, "Viktor") -> Subdivision("Supply"),
    Employee(3, "Igor") -> Subdivision("IT"),
    Employee(4, "Valera") -> Subdivision("IT"),
    Employee(5, "Sergey") -> Subdivision("Supply")
  )
  val employeeToSalary = employeeToSubdivision.mapValues(_ => r.nextInt(150000).RUR)
  val bonusPerSubdivision = subdivisionToPercent mapValues { p =>
    (salesPerYear ~* 30.~%) * p
  }

  bonusPerSubdivision foreach { t =>
    println(t._1)
    t._2.tracksPrint()
  }

  val subdivisionToEmployees = reverseMap(employeeToSubdivision)

  val total = subdivisionToEmployees map { t =>
    val subdivision = t._1
    val employees = t._2
    employees.map { employee =>
      employee -> employeeToSalary(employee).asScala ~+ (bonusPerSubdivision(subdivision) ~/ subdivisionToEmployees.keys.size)
    }
  }

  total foreach { i =>
    i foreach { t =>
      println(t._1)
      t._2.tracksPrint()
    }
  }

}
