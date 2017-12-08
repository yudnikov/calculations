package ru.yudnikov

import java.io.{PrintStream, PrintWriter}

import language.{implicitConversions, postfixOps}
import org.joda.money.{CurrencyUnit, Money}
import java.math.RoundingMode

import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}

package object calculations {

  implicit class MoneyExt(value: Money)(implicit context: CalculationsContext) {
    def asScala: ScalaMoney = ScalaMoney(value)
  }

  implicit class DoubleExt(value: Double) {
    def RUR: Money = Money.of(CurrencyUnit.of("RUR"), value)
    def USD: Money = Money.of(CurrencyUnit.USD, value)
    def EUR: Money = Money.of(CurrencyUnit.EUR, value)
    def ~ : FactorCalc = FactorCalc(value)
    def ~*(that: Double): FactorCalc = FactorCalc(this.value) * FactorCalc(that)
    def ~*(that: ScalaMoney)(implicit context: CalculationsContext): MoneyCalc = this.~ * that.~
    def ~+(that: Double): FactorCalc = FactorCalc(this.value) + FactorCalc(that)
    def ~% : PercentCalc = PercentCalc(BigDecimal(value))
  }

  implicit class ScalaMoney(val money: Money)(implicit context: CalculationsContext) {
    def to(currencyUnit: CurrencyUnit): ScalaMoney = {
      val rate = context.currencyRateProvider.getRate(money.getCurrencyUnit, currencyUnit).bigDecimal
      money.multipliedBy(rate, context.roundingMode).withCurrencyUnit(currencyUnit)
    }
    def in(currencyUnit: CurrencyUnit): ScalaMoney = {
      this.money.withCurrencyUnit(currencyUnit)
    }
    def *(bigDecimal: BigDecimal): ScalaMoney = {
      money.multipliedBy(bigDecimal.bigDecimal, context.roundingMode)
    }
    def /(bigDecimal: BigDecimal): ScalaMoney = {
      money.dividedBy(bigDecimal.bigDecimal, context.roundingMode)
    }
    def +(that: ScalaMoney): ScalaMoney = {
      this.money plus (that.money to this.money.getCurrencyUnit).money
    }
    def -(that: ScalaMoney): ScalaMoney = {
      this.money minus (that.money to this.money.getCurrencyUnit).money
    }
    def ~ : MoneyCalc = {
      MoneyCalc(this)
    }
    def ~*(that: FactorCalc): MoneyCalc = this.~ * that
    def ~*(that: Double): MoneyCalc = this.~ * that.~
    //def ~*(that: PercentCalc): MoneyCalc = this.~ * that
    def ~+(that: ScalaMoney): MoneyCalc = this.~ + that.~
    lazy val currencyUnit: CurrencyUnit = money.getCurrencyUnit
    override def toString: String = money.toString

    override def hashCode(): Int = money.hashCode()

    override def equals(obj: scala.Any): Boolean = obj match {
      case that: ScalaMoney =>
        this.money == that.money
      case _ =>
        false
    }
  }

  object ScalaMoney {
    def unapply(scalaMoney: ScalaMoney): Option[Money] = Some(scalaMoney.money)
  }

  trait CurrencyRateProvider {
    val dateTime: DateTime
    def getRate(numerator: CurrencyUnit, denominator: CurrencyUnit): BigDecimal
  }

  trait Operation {
    def spaced: String = " " + toString + " "
  }

  object Operation {
    case object PRIMARY extends Operation
    case object Conversion extends Operation
    case object + extends Operation
    case object - extends Operation
    case object * extends Operation
    case object / extends Operation
  }

  trait Calculation[T] {
    val value: T
    val operation: Operation
    val children: List[Calculation[_]]
    lazy val isFather: Boolean = isParent && children.forall(!_.isParent)
    lazy val isParent: Boolean = children.nonEmpty
    lazy val height: Int = if (!isParent) 0 else children.map(_.height).max + 1
    def representation: String = value.toString

    def trackFull(isRoot: Boolean = true): String = if (isRoot) {
      s"$value = (${children.map(_.trackFull(false)).mkString(operation.spaced)})"
    } else if (children.nonEmpty) {
      "(" + children.map(_.trackFull(false)).mkString(operation.spaced) + ")"
    } else {
      value.toString
    }

    def tracksPrint(printWriter: PrintStream = System.out): Unit = {
      tracks foreach printWriter.println
    }

    def tracks: List[String] = trackWithHeights().sortBy(_._2).map(t => t._2 + ") " + t._1)

    private def trackWithHeight: (String, Int) = s"$value = ${children.map(_.representation).mkString(operation.spaced)}" ->  height

    private def trackWithHeights(isRoot: Boolean = true): List[(String, Int)] = {
      if (isFather) {
        List(trackWithHeight)
      } else if (isParent) {
        trackWithHeight :: children.flatMap(_.trackWithHeights()) distinct
      } else
        Nil
    }
    lazy val isTaxAmount: Boolean = {
      children.exists(_.isInstanceOf[TaxCalc])
    }
    override def hashCode(): Int = 41 * value.hashCode() * operation.hashCode() * children.hashCode()
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: Calculation[_] if this.getClass == that.getClass =>
        this.value == that.value && this.operation == that.operation && this.children == that.children
      case _ =>
        false
    }
  }

  class MoneyCalc(val value: ScalaMoney, val operation: Operation = Operation.PRIMARY, val children: List[Calculation[_]] = Nil)
    (implicit context: CalculationsContext)
    extends Calculation[ScalaMoney] {
    def to(currencyUnit: CurrencyUnit): MoneyCalc = {
      this.value.currencyUnit != currencyUnit toOption {
        val rate = context.currencyRateProvider.getRate(value.money.getCurrencyUnit, currencyUnit)
        val factorCalc = ConversionFactorCalc(rate, context.currencyRateProvider, this.value.currencyUnit, currencyUnit)
        MoneyCalc((value * rate) in currencyUnit, Operation.*, this :: factorCalc :: Nil)
      } getOrElse this
    }
    def +(that: MoneyCalc): MoneyCalc = {
      val converted = that to this.value.currencyUnit
      val result = value + converted.value
      MoneyCalc(result, Operation.+, this :: converted :: Nil)
    }
    def -(that: MoneyCalc): MoneyCalc = {
      val converted = that to this.value.currencyUnit
      val result = value - converted.value
      MoneyCalc(result, Operation.-, this :: converted :: Nil)
    }
    def ~*(that: Double): MoneyCalc = {
      this * that.~
    }
    def ~+(that: ScalaMoney): MoneyCalc = {
      this + that.~
    }
    def ~-(that: ScalaMoney): MoneyCalc = {
      this - that.~
    }
    def *(factorCalc: FactorCalc): MoneyCalc = {
      val result = this.value * factorCalc.value
      MoneyCalc(result, Operation.*, this :: factorCalc :: Nil)
    }
    def *(percentCalc: PercentCalc): MoneyCalc = {
      val result = this.value * percentCalc.value
      MoneyCalc(result, Operation.*, this :: percentCalc :: Nil)
    }
    def *(taxCode: TaxCode): MoneyCalc = {
      this * TaxCalc(taxCode)
    }
    def *+(taxCode: TaxCode): MoneyCalc = {
      this + (this * TaxCalc(taxCode))
    }
  }
  object MoneyCalc {
    def apply(
      value: ScalaMoney,
      operation: Operation = Operation.PRIMARY,
      children: List[Calculation[_]] = Nil
    )(implicit context: CalculationsContext): MoneyCalc = new MoneyCalc(value, operation, children)
    def unapply(moneyCalc: MoneyCalc): Option[(ScalaMoney, Operation, List[Calculation[_]])] = {
      Some(moneyCalc.value, moneyCalc.operation, moneyCalc.children)
    }
  }

  implicit class BooleanExt(value: Boolean) {
    def toOption[T](body: => T): Option[T] = if (value) Some(body) else None
  }

  class FactorCalc(
    val value: BigDecimal,
    val operation: Operation = Operation.PRIMARY,
    val children: List[Calculation[_]] = Nil
  ) extends Calculation[BigDecimal]
      with Addendum[FactorCalc, FactorCalc]
      with Multiplier[FactorCalc, FactorCalc] {
    override def +(that: FactorCalc): FactorCalc = {
      new FactorCalc(this.value + that.value, Operation.+, this :: that :: Nil)
    }
    override def -(that: FactorCalc): FactorCalc = {
      new FactorCalc(this.value - that.value, Operation.-, this :: that :: Nil)
    }
    override def *(that: FactorCalc): FactorCalc = {
      new FactorCalc(this.value * that.value, Operation.*, this :: that :: Nil)
    }
    def *(that: MoneyCalc)(implicit context: CalculationsContext) = {
      val result = that.value * value
      MoneyCalc(result, Operation.*, this :: that :: Nil)
    }
    def ~*(that: Double): FactorCalc = {
      new FactorCalc(this.value * that, Operation.*, this :: that.~ :: Nil)
    }
    //def ~*(that: Double): FactorCalc = ???
    override def equals(obj: Any): Boolean = {
      super.equals(obj)
    }
  }

  object FactorCalc {
    def apply(
      value: BigDecimal,
      operation: Operation = Operation.PRIMARY,
      children: List[Calculation[_]] = Nil
    ): FactorCalc = new FactorCalc(value, operation, children)
  }

  case class ConversionFactorCalc(
    value: BigDecimal,
    provider: CurrencyRateProvider,
    numerator: CurrencyUnit,
    denominator: CurrencyUnit
  )(
    implicit formatter: DateTimeFormatter = DateTimeFormat.forPattern("yyyy-MM-dd")
  ) extends Calculation[BigDecimal] {
    override lazy val operation: Operation = Operation.Conversion
    override lazy val children: List[Calculation[_]] = Nil
    override lazy val representation: String = trackFull()
    override def trackFull(isRoot: Boolean): String = {
      s"$value | $numerator->$denominator @ ${formatter.print(provider.dateTime)} by ${provider.getClass.getSimpleName}"
    }
  }

  class PercentCalc(percent: BigDecimal, operation: Operation = Operation.PRIMARY, children: List[Calculation[_]] = Nil)
    extends FactorCalc(percent / 100, operation, children) {
    override def representation: String = s"$value%"
  }

  object PercentCalc {
    def apply(percent: BigDecimal, operation: Operation = Operation.PRIMARY, children: List[Calculation[_]] = Nil): PercentCalc = {
      new PercentCalc(percent, operation, children)
    }
  }

  trait Addendum[A, B] {
    def +(that: B): A
    def -(that: B): A
  }

  trait Multiplier[A, B] {
    def *(that: B): A
  }

  case class TaxCalc(taxCode: TaxCode)(implicit context: CalculationsContext)
    extends PercentCalc(context.taxRateProvider.getRate(taxCode)) {
    override def representation: String = s"$taxCode " + super.representation
  }

  abstract class TaxCode(val code: String)
  object TaxCode {
    case object VAT extends TaxCode("VAT")
  }

  trait TaxRateProvider {
    val dateTime: DateTime
    def getRate(taxCode: TaxCode): BigDecimal
  }

  case class CalculationsContext (
    currencyRateProvider: CurrencyRateProvider,
    taxRateProvider: TaxRateProvider,
    roundingMode: RoundingMode
  )

}
