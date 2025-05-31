package com.github.kright

import DoubleDouble.given_Fractional_DoubleDouble.mkNumericOps

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class DoubleDoubleTest extends AnyFunSuiteLike with ScalaCheckPropertyChecks:
  test("test mul") {
    val a = DoubleDouble.mul(1.0000000000001, 1.00000000000001)
    println(a)
  }

  test("oneThird") {
    val oneThird = DoubleDouble(1.0) / DoubleDouble(3.0)
    println(oneThird)
    println(binaryRepr(oneThird.high))
    println(binaryRepr(oneThird.low))

    val oneBack = oneThird + oneThird + oneThird
    println(oneBack)
  }

  def binaryRepr(d: Double): String =
    val binaryString = java.lang.Long.toBinaryString(java.lang.Double.doubleToLongBits(d)).reverse.padTo(64, '0').reverse
    s"${if (binaryString(0) == '0') "+" else "-"} ${binaryString.substring(1, 12)} 1.${binaryString.substring(12)}"
