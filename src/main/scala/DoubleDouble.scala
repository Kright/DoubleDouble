package com.github.kright

import DoubleDouble.given_Fractional_DoubleDouble.mkNumericOps

case class DoubleDouble(high: Double, low: Double):
  def isNan: Boolean = high.isNaN

  inline def >(y: 0): Boolean = high > 0.0
  inline def <(y: 0): Boolean = high < 0.0
  inline def ==(y: 0): Boolean = high == 0.0

  def pow(n: Int): DoubleDouble =
    if (n < 0) return (DoubleDouble.one / this).pow(-n)
    if (n == 0) return DoubleDouble.one
    positivePow(n)

  private def positivePow(n: Int): DoubleDouble =
    if (n == 1) return this
    val r = positivePow(n / 2)
    val r2 = r * r
    if (n % 2 == 0) r2 else r2 * this

  def sqrt: DoubleDouble =
    if (this < 0) return DoubleDouble.Nan
    if (this.high == 0.0) return DoubleDouble.zero

    val c = Math.sqrt(high)
    val u = DoubleDouble.mul(c, c)
    val cc = (high - u.high - u.low + low) * 0.5 / c
    DoubleDouble.normalized(c, cc)


object DoubleDouble:
  val Nan: DoubleDouble = DoubleDouble(Double.NaN, Double.NaN)
  val zero: DoubleDouble = DoubleDouble(0.0, 0.0)
  val one: DoubleDouble = DoubleDouble(1.0, 0.0)
  val minusOne: DoubleDouble = DoubleDouble(-1.0, 0.0)
  val ten: DoubleDouble = DoubleDouble(10.0, 0.0)

  val splitConst: Double = 134217729.0D // 2 ^ 27 + 1

  def apply(x: Double): DoubleDouble = DoubleDouble(x, 0.0)

  def add(x: Double, y: Double): DoubleDouble =
    if (Math.abs(x) > Math.abs(y)) normalized(x, y)
    else normalized(y, x)

  def normalized(bigger: Double, smaller: Double): DoubleDouble =
    val z = bigger + smaller
    val zz = bigger - z + smaller
    DoubleDouble(z, zz)

  /**
   * @return non-normalized DoubleDouble
   */
  def split(x: Double): DoubleDouble =
    val p = x * splitConst
    val h = x - p + p
    val l = x - h
    DoubleDouble(h, l)

  def mul(x: Double, y: Double): DoubleDouble =
    val sx = split(x)
    val sy = split(y)

    val p = sx.high * sy.high
    val q = sx.high * sy.low + sx.low * sy.high
    val z = p + q
    val zz = p - z + q + sx.low * sy.low

    DoubleDouble(z, zz)

  def parseString(str: String): Option[DoubleDouble] = {
    case class StringParts(isPositive: Boolean,
                           numbersBeforePoint: String,
                           numbersAfterPoint: String,
                           isPositiveExponent: Boolean,
                           exponentNumbers: String)

    def consumeSign(str: String): (Boolean, String) =
      val isPositive = !str.startsWith("-")
      val remainingStr = if (str.startsWith("+") || str.startsWith("-")) str.substring(1) else str
      (isPositive, remainingStr)

    def splitAtPoint(str: String): Option[(String, String)] =
      val parts = str.split("\\.")
      if (parts.length > 2) return None
      Some(
        if (parts.length == 1) (parts(0), "")
        else (parts(0), parts(1))
      )

    def parseStringParts(str: String): Option[StringParts] = {
      val (isPositive, remainingStr) = consumeSign(str)

      val arr = remainingStr.split("E")
      if (arr.length == 1) {
        return for ((beforePoint, afterPoint) <- splitAtPoint(arr(0)))
          yield StringParts(isPositive, beforePoint, afterPoint, true, "")
      }
      if (arr.length == 2) {
        val (exponentIsPositive, exponentNumbers) = consumeSign(arr(1))
        return for ((beforePoint, afterPoint) <- splitAtPoint(arr(0)))
          yield StringParts(isPositive, beforePoint, afterPoint, exponentIsPositive, exponentNumbers)
      }
      None
    }

    def toNumber(parts: StringParts): Option[DoubleDouble] =
      if (parts.numbersBeforePoint.isEmpty && parts.numbersAfterPoint.isEmpty) {
        return None
      }

      if (parts.numbersBeforePoint.exists(c => !c.isDigit) ||
        parts.numbersAfterPoint.exists(c => !c.isDigit) ||
        parts.exponentNumbers.exists(c => !c.isDigit)
      ) return None

      var result: DoubleDouble = DoubleDouble.one

      for (v <- if (parts.numbersBeforePoint.isEmpty) "0" else parts.numbersBeforePoint) {
        val digit = v - '0'
        result = result * DoubleDouble.ten + DoubleDouble(digit)
      }

      for ((v, i) <- (if (parts.numbersAfterPoint.isEmpty) "0" else parts.numbersAfterPoint).zipWithIndex) {
        val digit = v - '0'
        result += DoubleDouble.ten.pow(-i) * DoubleDouble(digit)
      }

      if (!parts.isPositive) {
        result = -result
      }

      if (parts.exponentNumbers.nonEmpty) {
        var exponent = parts.exponentNumbers.toInt
        if (!parts.isPositiveExponent) {
          exponent = -exponent
        }

        result *= DoubleDouble.ten.pow(exponent)
      }

      Option(result)

    parseStringParts(str).flatMap(toNumber)
  }

  given Fractional[DoubleDouble] with {

    override def plus(x: DoubleDouble, y: DoubleDouble): DoubleDouble =
      val r = x.high + y.high
      val s = if (Math.abs(x.high) > Math.abs(y.high)) {
        x.high - r + y.high + y.low + x.low
      } else {
        y.high - r + x.high + x.low + y.low
      }
      normalized(r, s)

    override def minus(x: DoubleDouble, y: DoubleDouble): DoubleDouble = x + (-y)

    override def times(x: DoubleDouble, y: DoubleDouble): DoubleDouble =
      val r = mul(x.high, y.high)
      val c = r.high
      val cc = x.high * y.low + x.low * y.high + r.low
      normalized(c, cc)

    override def div(x: DoubleDouble, y: DoubleDouble): DoubleDouble =
      val c = x.high / y.high
      val u = mul(c, y.high)
      val cc = (x.high - u.high - u.low + x.low - c * y.low) / y.high
      normalized(c, cc)

    override def negate(x: DoubleDouble): DoubleDouble = DoubleDouble(-x.high, -x.low)

    override def fromInt(x: Int): DoubleDouble = DoubleDouble(x, 0.0)

    override def toInt(x: DoubleDouble): Int = toDouble(x).toInt

    override def toLong(x: DoubleDouble): Long = ???

    override def toFloat(x: DoubleDouble): Float = toDouble(x).toFloat

    override def toDouble(x: DoubleDouble): Double = x.high + x.low

    override def compare(x: DoubleDouble, y: DoubleDouble): Int = {
      if (x.high > y.high) return 1
      if (x.high < y.high) return -1
      if (x.low > y.low) return 1
      if (x.low < y.low) return -1
      0
    }

    override def zero: DoubleDouble = DoubleDouble.zero

    override def one: DoubleDouble = DoubleDouble.one

    override def abs(x: DoubleDouble): DoubleDouble = if (x < 0) -x else x

    override def sign(x: DoubleDouble): DoubleDouble = {
      if (x.high > 0.0) return DoubleDouble.one
      if (x.high < 0.0) return DoubleDouble.minusOne
      if (x.low > 0.0) return DoubleDouble.one
      if (x.low < 0.0) return DoubleDouble.minusOne
      DoubleDouble.zero
    }

    override def parseString(str: String): Option[DoubleDouble] = DoubleDouble.parseString(str)
  }
