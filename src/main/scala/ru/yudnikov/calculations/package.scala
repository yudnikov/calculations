package ru.yudnikov

import language.{implicitConversions, postfixOps}
import org.joda.money.{CurrencyUnit, Money}
import java.math.RoundingMode

import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}

package object calculations {

  implicit class MoneyExt(value: Money)(implicit currencyRateProvider: CurrencyRateProvider, roundingMode: RoundingMode) {
    def asScala: ScalaMoney = ScalaMoney(value)
  }

  implicit class DoubleExt(value: Double) {
    def RUR: Money = Money.of(CurrencyUnit.of("RUR"), value)
    def USD: Money = Money.of(CurrencyUnit.USD, value)
    def EUR: Money = Money.of(CurrencyUnit.EUR, value)
  }

  implicit class ScalaMoney(val money: Money)(implicit currencyRateProvider: CurrencyRateProvider, roundingMode: RoundingMode) {
    def to(currencyUnit: CurrencyUnit): ScalaMoney = {
      val rate = currencyRateProvider.getRate(money.getCurrencyUnit, currencyUnit).bigDecimal
      money.multipliedBy(rate, roundingMode).withCurrencyUnit(currencyUnit)
    }
    def in(currencyUnit: CurrencyUnit): ScalaMoney = {
      this.money.withCurrencyUnit(currencyUnit)
    }
    def *(bigDecimal: BigDecimal): ScalaMoney = {
      money.multipliedBy(bigDecimal.bigDecimal, roundingMode)
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
    lazy val currencyUnit: CurrencyUnit = money.getCurrencyUnit
    override def toString: String = money.toString
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
  }

  trait Calculation[T] {
    val value: T
    val operation: Operation
    val children: List[Calculation[_]]
    def trace(isRoot: Boolean = true): String = if (isRoot) {
      s"$value = (${children.map(_.trace(false)).mkString(operation.spaced)})"
    } else if (children.nonEmpty) {
      "(" + children.map(_.trace(false)).mkString(operation.spaced) + ")"
    } else {
      value.toString
    }

    lazy val height: Int = if (!isParent) 0 else {
      children.map(_.height).max + 1
    }

    def tracePrint(isRoot: Boolean = true): List[(String, Int)] = {
      if (isFather) {
        List(traceShort)
      } else if (isParent) {
        children.flatMap(_.tracePrint()) union List(traceShort)
      } else
        Nil
    }
    def isParent: Boolean = children.nonEmpty
    def isFather: Boolean = {
      val res = isParent && children.forall(!_.isParent)
      res
    }
    def traceShort: (String, Int) = s"$value = ${children.map(_.representation).mkString(operation.spaced)}" -> height
    def representation: String = value.toString
  }

  case class MoneyCalc(value: ScalaMoney, operation: Operation = Operation.PRIMARY, children: List[Calculation[_]] = Nil)
    (implicit currencyRateProvider: CurrencyRateProvider, roundingMode: RoundingMode)
    extends Calculation[ScalaMoney] {
    def to(currencyUnit: CurrencyUnit): MoneyCalc = {
      this.value.currencyUnit != currencyUnit toOption {
        val rate = currencyRateProvider.getRate(value.money.getCurrencyUnit, currencyUnit)
        val factorCalc = ConversionFactorCalc(rate, currencyRateProvider, this.value.currencyUnit, currencyUnit)
        MoneyCalc((value * rate) in currencyUnit, Operation.*, this :: factorCalc :: Nil)
      } getOrElse this
    }
    def +(that: MoneyCalc): MoneyCalc = {
      val converted = that to this.value.currencyUnit
      val result = value + converted.value
      MoneyCalc(result, Operation.+, this :: converted :: Nil)
    }
    def +(that: ScalaMoney): MoneyCalc = {
      this + that.~
    }
    def -(that: MoneyCalc): MoneyCalc = {
      val converted = that to this.value.currencyUnit
      val result = value - converted.value
      MoneyCalc(result, Operation.-, this :: converted :: Nil)
    }
    def -(that: ScalaMoney): MoneyCalc = {
      this - that.~
    }
  }

  implicit class BooleanExt(value: Boolean) {
    def toOption[T](body: => T): Option[T] = if (value) Some(body) else None
  }

  case class FactorCalc(value: BigDecimal, operation: Operation = Operation.PRIMARY, children: List[Calculation[_]] = Nil)
    extends Calculation[BigDecimal] {
  }

  case class ConversionFactorCalc(
    value: BigDecimal,
    provider: CurrencyRateProvider,
    numerator: CurrencyUnit,
    denominator: CurrencyUnit
  )(
    implicit formatter: DateTimeFormatter = DateTimeFormat.forPattern("yyyy-MM-dd")
  ) extends Calculation[BigDecimal] {
    override val operation: Operation = Operation.Conversion
    override val children: List[Calculation[_]] = Nil
    override def trace(isRoot: Boolean): String = {
      s"$value | $numerator->$denominator @ ${formatter.print(provider.dateTime)} by ${provider.getClass.getSimpleName}"
    }
    override def representation: String = trace()
  }

}
