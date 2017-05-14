package net.totietje.complex

import net.totietje.complex.Complex._

import scala.math.BigDecimal.RoundingMode

import scala.language.implicitConversions

/** A class which represents a complex number, that is, a number in the form `a + bi`,
  * where i is one of the two square roots of -1.
  *
  * `a` is called the real part, and `b` is called the imaginary part.
  * @param re
  *           The real part
  * @param im
  *           The imaginary part
  */
case class Complex(re: Double, im: Double = 0) {
  
  /** Adds this complex number to another one and returns the result.
    *
    * This implements the formula: {{{(a + bi) + (c + di) = (a + c) + (b + d)i}}}
    * @param that
    *             The complex number to be added to this one
    * @return
    *         `this + that`
    */
  def +(that: Complex): Complex = Complex(re + that.re, im + that.im)
  
  /** Subtracts another complex number from this one and returns the result.
    *
    * This implements the formula: {{{(a + bi) - (c + di) = (a - c) + (b - d)i}}}
    * @param that
    *             The complex number to be subtracted from this one
    * @return
    *         `this - that`
    */
  def -(that: Complex): Complex = Complex(re - that.re, im - that.im)
  
  /** Multiplies this complex number by another one and returns the result.
    *
    * This implements the formula: {{{(a + bi) * (c + di) = (ac - bd) + (ad + bc)}}}
    * @param that
    *             The complex number to be multiplied with this one
    * @return
    *         `this * that`
    */
  def *(that: Complex): Complex = {
    val realPart = re * that.re - im * that.im
    val imaginaryPart = re * that.im + that.re * im
    Complex(realPart, imaginaryPart)
  }
  
  /** Divides this complex number by another one and returns the result.
    *
    * This implements the formula: {{{(a + bi) / (c + di) = (a + bi)(c - di) / (c^2 + d^2)}}}
    *
    * The divisor is then a real number, and thus it is easy to compute the result when
    * the nominator is divided by it.
    *
    * If the divisor is 0, the result will be NaN + NaNi.
    * @param that
    *             The complex number by which this one is to be divided
    * @return
    *         `this / that`
    */
  def /(that: Complex): Complex = {
    val top = this * that.conj
    val bottom = that.re * that.re + that.im * that.im
    Complex(top.re / bottom, top.im / bottom)
  }
  
  /** Calculates and returns the negative of this complex number.
    *
    * This implements the formula: {{{-(a + bi) = -a - bi}}}
    * @return
    *         `-a - bi`
    */
  def unary_- = Complex(-re, -im)
  
  /** Returns the complex conjugate of this number, that is, a complex number with the same
    * real part but with the imaginary part negated.
    *
    * The conjugate of `a + bi`, therefore, is `a - bi`
    * @return
    *         The complex conjugate
    */
  def conj: Complex = Complex(re, -im)
  
  /** Calculates `e` to the power of this complex number and returns the result.
    *
    * This implements the formula: {{{e ^ (a + bi) = (e ^ a) * (cos(b) + i * sin(b)}}}
    * @return
    *         `e`^`this`^
    */
  def exp: Complex = math.exp(re) * (math.cos(im) + (I * math.sin(im)))
  
  /** Calculates the absolute value of this complex number, also known as the modulus, and returns it.
    * The absolute is the distance between a point on the complex plane representing the number and the origin.
    *
    * This implements the formula: {{{|a + bi| = √(a^2 + b^2)}}}
    * where `|z|` denotes the absolute of `z`.
    *
    * If either the real part of the imaginary part, or both, are infinite, the result will be positive infinity.
    * @return
    *         The absolute
    */
  def abs: Double = math.sqrt(im * im + re * re)
  
  /** Calculates the argument, that is, the angle between the positive real axis and the point representing
    * this number on the complex plane. The result will be between `π` and `-π`.
    *
    * NaN and infinite values are handled by `math.atan2(Double, Double)`.
    *
    * @return
    *         The argument
    */
  def arg: Double = math.atan2(im, re)
  
  /**
    * Calculates the principle natural logarithm of this number. Although numbers have infinitely many logarithms,
    * separated from each-other by `2πi`, this will only return the one with its imaginary part in the range
    * `[-π, π]`.
    *
    * If the input is 0, the result will be negative infinity.
    *
    * @return
    *         `logₑ(this)`
    */
  def log: Complex = this match {
    case Zero => Complex(Double.NegativeInfinity)
    case _    => Complex(math.log(abs), arg)
  }
  
  /** Raises this number to the power of another and returns the result.
    *
    * This implements the formula: {{{a ^ b = e ^ (b * logₑ(a))}}}
    *
    * When this number is 0, then:<br>
    * - if the real part of the exponent is greater than 0, then the result will be 0<br>
    * - otherwise, the result will be NaN.
    *
    * Note that this is a multivalued function, so there will be multiple possible results. This will
    * return the principle value, as given by the principle value of `logₑ(a)`.
    * @param that
    *             The exponent
    * @return
    *         `this`^`that`^
    */
  def pow(that: Complex): Complex = (this, that) match {
    case (Zero, Complex(a, _)) if a > 0 => Zero
    case (Zero, _) => NaN
    case (a, b) => (a.log * b).exp
  }
  
  /** Raises this number to the power of another and returns the result.
    *
    * This implements the formula: {{{a ^ b = e ^ (b * logₑ(a))}}}
    *
    * When this number is 0, then:<br>
    * - if the real part of the exponent is greater than 0, then the result will be 0<br>
    * - otherwise, the result will be NaN.
    *
    * Note that this is a multivalued function, so there will be multiple possible results. This will
    * return the principle value, as given by the principle value of `logₑ(a)`.
    *
    * @param that
    *             The exponent
    * @return
    *         `this`^`that`^
    */
  def ~^(that: Complex): Complex = pow(that)
  
  /** Returns the square root of this number, as given by `pow(0.5)`.
    *
    * Every number apart from 0 has two square roots. The other square root will be the negative of this one.
    * The returned square root will be the one whose real part is positive.
    * @return
    *         `√this`
    */
  def sqrt: Complex = pow(0.5)
  
  /** Returns the sine of this number.
    *
    * This implements the formula: {{{sin(z) = i(e^(-iz) - e^(iz)) / 2}}}
    * @return
    *         `sin(this)`
    */
  def sin: Complex = {
    0.5 * I * ((-I * this).exp - (I * this).exp)
  }
  
  /** Returns the principal inverse sine of this number, that is, the number whose sine this is.
    *
    * This implements the formula: {{{asin(z) = -i * log(iz + √(1 - z^2))}}}
    * @return
    *         `sin`^`-1`^`(this)`
    */
  def asin: Complex = -I * (I * this + (1 - this * this).sqrt).log
  
  /** Returns the cosine of this number.
    *
    * This implements the formula: {{{cos(z) = (e^(iz) + e^(-iz)) / 2}}}
    * @return
    *         `cos(this)`
    */
  def cos: Complex = 0.5 * ((I * this).exp + (-I * this).exp)
  
  /** Returns the principal inverse cosine of this number, that is, the number whose cosine this is.
    *
    * This implements the formula: {{{acos(z) = 1/2 * π - asin(z)}}}
    * @return
    *         `cos`^`-1`^`(this)`
    */
  def acos: Complex = 0.5 * Pi - asin
  
  /** Returns the tangent of this number.
    *
    * This implements the formula: {{{tan(z) = i(1 - e^(2iz)) / (e^(2iz) + 1)}}}
    *
    * This will return NaN where the tan function is undefined, which is where {{{e^(2iz) + 1 = 0}}}
    * @return
    *         `tan(this)`
    */
  def tan: Complex = {
    val exponential = (2 * I * this).exp
    val bottom = I * (exponential + 1)
    
    if (bottom.round(15) == Complex(0)) {
      NaN
    } else {
      I * (1 - exponential) / bottom
    }
  }
  
  /** Returns the principal inverse tangent of this number, that is, the number whose tangent this is.
    *
    * This implements the formula: {{{atan(z) = i(log(1 - iz) - log(1 + iz)) / 2}}}
    * @return
    *         `tan`^`-1`^`(this)`
    */
  def atan: Complex = {
    val iMult = I * this
    0.5 * I * ((1 - iMult).log - (1 + iMult).log)
  }
  
  /** Returns the hyperbolic sine of this number.
    *
    * This implements the formula: {{{sinh(z) = -i * sin(iz)}}}
    * @return
    *         `sinh(this)`
    */
  def sinh: Complex = -I * (I * this).sin
  
  /** Returns the principal inverse hyperbolic sine of this number, that is, the number whose hyperbolic sine this is.
    *
    * This implements the formula: {{{asinh(z) = -i * asin(iz)}}}
    * @return
    *         `sinh`^`-1`^`(this)`
    */
  def asinh: Complex = -I * (I * this).asin
  
  /** Returns the hyperbolic cosine of this number.
    *
    * This implements the formula: {{{cosh(z) = cos(iz)}}}
    * @return
    *         `cosh(this)`
    */
  def cosh: Complex = (I * this).cos
  
  /** Returns the principal inverse hyperbolic cosine of this number, that is, the number whose hyperbolic
    * cosine this is.
    *
    * This implements the formula: {{{acosh(z) = log(z + √(z + 1) * √(z - 1))}}}
    * @return
    *         `cosh`^`-1`^`(this)`
    */
  def acosh: Complex = (this + (this + 1).sqrt * (this - 1).sqrt).log
  
  /** Returns the hyperbolic tangent of this number.
    *
    * This implements the formula: {{{tanh(z) = -i * tan(iz)}}}
    * @return
    *         `tanh(this)`
    */
  def tanh: Complex = -I * (I * this).tan
  
  /** Returns the principal inverse hyperbolic tangent of this number, that is, the number whose hyperbolic
    * tangent this is.
    *
    * This implements the formula: {{{atanh(z) = -i * atan(iz)}}}
    * @return
    *         `tanh`^`-1`^`(this)`
    */
  def atanh: Complex = -I * (I * this).atan
  
  /**
    * Returns a string representation of this complex number.
    *
    * This will return a string in the format `a + bi` except in special cases.
    *
    * These are:
    * - If the real part is 0, the `a` will be omitted
    * - If the imaginary part is 0, the `+ bi` will be omitted
    * - If `b` is 1, it will be omitted
    * - If `b` is negative, the format will be `a - bi`.
    * @return
    *         A string representation of this complex number
    */
  override def toString: String = this match {
    case Complex(0, 0)                              => "0.0"
    case Complex(0, 1)                              => "i"
    case Complex(0, -1)                             => "-i"
    case Complex(0, imaginary)                      => s"${imaginary}i"
    case Complex(real, 0)                           => real.toString
    case Complex(real, 1)                           => s"$real + i"
    case Complex(real, -1)                          => s"$real - i"
    case Complex(real, imaginary) if imaginary < 0  => s"$real - ${-imaginary}i"
    case Complex(real, imaginary)                   => s"$real + ${imaginary}i"
  }
  
  /** Rounds this complex number to the given number of significant figures and returns the result.
    * @param precision
    *                  The number of significant figures to round to
    * @return
    *         The rounded complex number
    */
  def round(implicit precision: Int = 8): Complex = {
    Complex(round(re), round(im))
  }
  
  private def round(v: Double)(implicit precision: Int): Double = if (v.isInfinite || v.isNaN) {
    v
  } else {
    BigDecimal(v).setScale(precision, RoundingMode.HALF_UP).toDouble
  }
}

/** A companion object for the Complex class.
  *
  * This contains constants and implicit conversions.
  */
object Complex {
  /** One of the two square roots of -1
    *
    * This has real part 0 and imaginary part 1.
    */
  val I = Complex(0, 1)
  
  /** The number with real part 0 and imaginary part 0.
    */
  val Zero = Complex(0)
  
  /** The number with real part 1 and imaginary part 0.
    */
  val One = Complex(1)
  
  /** The base of the natural logarithm.
    */
  val E = Complex(math.E)
  
  /** Half of the one true circle constant.
    *
    * This is defined as the ratio of a circle's circumference to its diameter.
    */
  val Pi = Complex(math.Pi)
  
  /** The one true circle constant.
    *
    * This is defined as the ratio of a circle's circumference to its radius.
    *
    * It is exactly 2π and is denoted τ.
    */
  val Tau = Complex(math.Pi * 2)
  
  /** The number whose real and imaginary parts are both positive infinity.
    */
  val Infinity = Complex(Double.PositiveInfinity, Double.PositiveInfinity)
  
  /** Not a Number.
    *
    * This is the number whose real and imaginary parts are both NaN.
    */
  val NaN = Complex(Double.NaN, Double.NaN)
  
  /** Wraps a `Double` as a complex number.
    *
    * The output will have the input as its real part and 0 as its imaginary part.
    * @param v
    *          The number to convert to a complex number
    * @return
    *         `v` as a complex number
    */
  implicit def fromDouble(v: Double): Complex = Complex(v)
  
  /** Wraps an `Int` as a complex number.
    *
    * The output will have the input as its real part and 0 as its imaginary part.
    * @param v
    *          The number to convert to a complex number
    * @return
    *         `v` as a complex number
    */
  implicit def fromInt(v: Int): Complex = Complex(v)
}