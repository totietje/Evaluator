package net.totietje.complex

import net.totietje.complex.Complex._

import scala.math.BigDecimal.RoundingMode

case class Complex(re: Double, im: Double = 0) {
  def +(other: Complex): Complex = Complex(re + other.re, im + other.im)
  
  def -(other: Complex): Complex = Complex(re - other.re, im - other.im)
  
  def conj: Complex = Complex(re, -im)
  
  def *(other: Complex): Complex = {
    val realPart = re * other.re - im * other.im
    val imaginaryPart = re * other.im + other.re * im
    Complex(realPart, imaginaryPart)
  }
  
  def /(other: Complex): Complex = {
    val top = this * other.conj
    val bottom = other.re * other.re + other.im * other.im
    Complex(top.re / bottom, top.im / bottom)
  }
  
  def unary_- = Complex(-re, -im)
  
  def exp: Complex = math.exp(re) * (math.cos(im) + (I * math.sin(im)))
  
  def abs: Double = math.sqrt(im * im + re * re)
  
  def arg: Double = math.atan2(im, re)
  
  def log: Complex = Complex(math.log(abs), arg)
  
  def pow(other: Complex): Complex = (this, other) match {
    case (Zero, Zero) => NaN
    case (Zero, _) => Zero
    case (a, b) => (a.log * b).exp
  }
  
  def ~^(other: Complex): Complex = pow(other)
  
  def sqrt: Complex = pow(0.5)
  
  def sin: Complex = {
    0.5 * I * ((-I * this).exp - (I * this).exp)
  }
  
  def asin: Complex = -I * (I * this + (1 - this * this).sqrt).log
  
  def cos: Complex = 0.5 * ((I * this).exp + (-I * this).exp)
  
  def acos: Complex = 0.5 * Pi - asin
  
  def tan: Complex = {
    val exponential = (2 * I * this).exp
    val bottom = I * (exponential + 1)
    
    if (bottom.round == Complex(0)) {
      NaN
    } else {
      (exponential - 1) / bottom
    }
  }
  
  def atan: Complex = {
    val iMult = I * this
    0.5 * I * ((1 - iMult).log - (1 + iMult).log)
  }
  
  def sinh: Complex = -I * (I * this).sin
  
  def asinh: Complex = -I * (I * this).asin
  
  def cosh: Complex = (I * this).cos
  
  def acosh: Complex = (this + (this + 1).sqrt * (this - 1).sqrt).log
  
  def tanh: Complex = -I * (I * this).tan
  
  def atanh: Complex = -I * (I * this).atan
  
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
  
  def round(implicit precision: Int = 8): Complex = {
    Complex(round(re), round(im))
  }
  
  private def round(v: Double)(implicit precision: Int): Double = if (v.isInfinite || v.isNaN) {
    v
  } else {
    BigDecimal(v).setScale(precision, RoundingMode.HALF_DOWN).toDouble
  }
}

object Complex {
  val I = Complex(0, 1)
  val Zero = Complex(0)
  val One = Complex(1)
  
  val E = Complex(math.E)
  val Pi = Complex(math.Pi)
  val Tau = Complex(math.Pi * 2)
  
  val NaN = Complex(Double.NaN, Double.NaN)
  
  implicit def DoubleToDouble(Double: Double): Double = Double.toDouble
  implicit def fromDouble(v: Double): Complex = Complex(v)
  implicit def fromInt(v: Int): Complex = Complex(v)
}