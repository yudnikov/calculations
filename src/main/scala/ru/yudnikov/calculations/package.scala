package ru.yudnikov

import language.{implicitConversions, postfixOps}
import org.joda.money.{CurrencyUnit, Money}

import java.math.RoundingMode

package object calculations {

  implicit class MoneyExt(value: Money)(implicit currencyRateProvider: CurrencyRateProvider, roundingMode: RoundingMode) {
    def asScala: ScalaMoney = ScalaMoney(value)
  }

  implicit class DoubleExt(value: Double) {
    def RUR: Money = Money.of(CurrencyUnit.of("RUR"), value)
    def USD: Money = Money.of(CurrencyUnit.USD, value)
  }

  implicit class ScalaMoney(val money: Money)(implicit currencyRateProvider: CurrencyRateProvider, roundingMode: RoundingMode) {
    def to(currencyUnit: CurrencyUnit): ScalaMoney = {
      val rate = currencyRateProvider.getRate(money.getCurrencyUnit, currencyUnit).bigDecimal
      money.multipliedBy(rate, roundingMode).withCurrencyUnit(currencyUnit)
    }
    def +(that: ScalaMoney): ScalaMoney = {
      this.money plus (that.money to this.money.getCurrencyUnit).money
    }
    def -(that: ScalaMoney): ScalaMoney = {
      this.money minus (that.money to this.money.getCurrencyUnit).money
    }
    override def toString: String = money.toString
  }

  object ScalaMoney {
    def unapply(scalaMoney: ScalaMoney): Option[Money] = Some(scalaMoney.money)
  }

  trait CurrencyRateProvider {
    def getRate(numerator: CurrencyUnit, denominator: CurrencyUnit): BigDecimal
  }

}
