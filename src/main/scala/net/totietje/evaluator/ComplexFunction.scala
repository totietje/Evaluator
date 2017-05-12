package net.totietje.evaluator

import ComplexFunction._
import Complex._

import scala.math.BigDecimal

sealed trait ComplexFunction {
  def apply(in: Map[String, Complex] = Map()) : Complex
  
  def +(other: ComplexFunction) = Add(this, other)
  def -(other: ComplexFunction) = Subtract(this, other)
  def *(other: ComplexFunction) = Multiply(this, other)
  def /(other: ComplexFunction) = Divide(this, other)
  def ~^(other: ComplexFunction) = Power(this, other)
  def unary_- : ComplexFunction = UnaryMinus(this)
  
  override def toString: String = ComplexFunction.toString(this)
}

object ComplexFunction {
  implicit def toConstant(v: Complex): Constant = Constant(v)
  implicit def toConstant(v: Double): Constant = Constant(v)
  implicit def toConstant(v: Int): Constant = Constant(v)
  
  case class Variable(variable: String) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = in.get(variable) match {
      case Some(value) => value
      case None => throw new Exception(s"$variable is undefined.")
    }
  }
  
  case class Constant(z: Complex) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z
  }
  
  case class Add(a: ComplexFunction, b: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = a(in) + b(in)
  }
  
  case class Subtract(a: ComplexFunction, b: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = a(in) - b(in)
  }
  
  case class Multiply(a: ComplexFunction, b: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = a(in) * b(in)
  }
  
  case class Divide(a: ComplexFunction, b: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = a(in) / b(in)
  }
  
  case class Power(a: ComplexFunction, b: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = a(in) ~^ b(in)
  }
  
  case class UnaryMinus(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = -z(in)
  }
  
  case class Conj(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).conj
  }
  
  case class Abs(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).abs
  }
  
  case class Arg(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).arg
  }
  
  case class Sqrt(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).sqrt
  }
  
  case class Re(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).re
  }
  
  case class Im(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).im
  }
  
  case class Log(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).log
  }
  
  case class Sin(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).sin
  }
  
  case class Asin(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).asin
  }
  
  case class Cos(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).cos
  }
  
  case class Acos(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).acos
  }
  
  case class Tan(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).tan
  }
  
  case class Atan(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).atan
  }
  
  case class Sinh(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).sinh
  }
  
  case class Asinh(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).asinh
  }
  
  case class Cosh(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).cosh
  }
  
  case class Acosh(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).acosh
  }
  
  case class Tanh(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).tanh
  }
  
  case class Atanh(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).atanh
  }
  
  def differentiate(function: ComplexFunction)(implicit respect: String = "x") : ComplexFunction = {
    val derivative : ComplexFunction = simplify(function) match {
      case Variable(name) if name == respect => 1
      case Variable(_)    => 0
      case Constant(_)    => 0
      case Add(a, b)      => differentiate(a) + differentiate(b)
      case Subtract(a, b) => differentiate(a) - differentiate(b)
      case Multiply(a, b) => a * differentiate(b) + b * differentiate(a)
      case Divide(a, b)   => (b * differentiate(a) - a * differentiate(b)) / (b * b)
      case Power(a, b)    => (a ~^ b) * (differentiate(a) * b / a + differentiate(b) * Log(a))
      case UnaryMinus(z)  => -differentiate(z)
      case Conj(_)        => NaN
      case Abs(_)         => NaN
      case Arg(_)         => NaN
      case Im(_)          => NaN
      case Re(_)          => NaN
      case Sqrt(z)        => 0.5 * (z ~^ -0.5) * differentiate(z)
      case Log(z)         => differentiate(z) / z
      case Sin(z)         => Cos(z) * differentiate(z)
      case Asin(z)        => differentiate(z) / Sqrt(1 - (z ~^ 2))
      case Cos(z)         => -Sin(z) * differentiate(z)
      case Acos(z)        => differentiate(-Sin(z))
      case Tan(z)         => differentiate(z) / (Cos(z) ~^ 2)
      case Atan(z)        => differentiate(z) / (1 + (z ~^ 2))
      case Sinh(z)        => Cosh(z) * differentiate(z)
      case Asinh(z)       => differentiate(z) / Sqrt((z ~^ 2) + 1)
      case Cosh(z)        => Sinh(z) * differentiate(z)
      case Acosh(z)       => differentiate(z) / Sqrt((z ~^ 2) - 1)
      case Tanh(z)        => differentiate(z) / (Sinh(z) ~^ 2)
      case Atanh(z)       => differentiate(z) / (1 - (z ~^ 2))
    }
    
    simplify(derivative)
  }
  
  private def toString(function: ComplexFunction) : String = function match {
    case Variable(name) => name
    case Constant(E)    => "e"
    case Constant(Pi)   => "π"
    case Constant(Tau)  => "2 * π"
    case Constant(v)    => s"$v"
    case Add(a, b)      => s"($a + $b)"
    case Subtract(a, b) => s"($a - $b)"
    case Multiply(a, b) => s"($a * $b)"
    case Divide(a, b)   => s"($a / $b)"
    case Power(a, b)    => s"$a ^ $b"
    case UnaryMinus(z)  => s"-$z"
    case Conj(z)        => s"conj($z)"
    case Abs(z)         => s"abs($z)"
    case Arg(z)         => s"arg($z)"
    case Im(z)          => s"im($z)"
    case Re(z)          => s"re($z)"
    case Sqrt(z)        => s"sqrt($z)"
    case Log(z)         => s"log($z)"
    case Sin(z)         => s"sin($z)"
    case Asin(z)        => s"asin($z)"
    case Cos(z)         => s"cos($z)"
    case Acos(z)        => s"acos($z)"
    case Tan(z)         => s"tan($z)"
    case Atan(z)        => s"atan($z)"
    case Sinh(z)        => s"sinh($z)"
    case Asinh(z)       => s"asinh($z)"
    case Cosh(z)        => s"cosh($z)"
    case Acosh(z)       => s"acosh($z)"
    case Tanh(z)        => s"tanh($z)"
    case Atanh(z)       => s"atanh($z)"
  }
  
  def simplify(function: ComplexFunction) : ComplexFunction = simplifications(function) match {
    case Add(a, b)      => simplifications(simplify(a) + simplify(b))
    case Subtract(a, b) => simplifications(simplify(a) - simplify(b))
    case Multiply(a, b) => simplifications(simplify(a) * simplify(b))
    case Divide(a, b)   => simplifications(simplify(a) / simplify(b))
    case Power(a, b)    => simplifications(simplify(a) ~^ simplify(b))
    case UnaryMinus(z)  => UnaryMinus(simplify(z))
    case Conj(z)        => Conj(simplify(z))
    case Abs(z)         => Abs(simplify(z))
    case Arg(z)         => Arg(simplify(z))
    case Im(z)          => Im(simplify(z))
    case Re(z)          => Re(simplify(z))
    case Sqrt(z)        => Sqrt(simplify(z))
    case Log(z)         => Log(simplify(z))
    case Sin(z)         => Sin(simplify(z))
    case Asin(z)        => Asin(simplify(z))
    case Cos(z)         => Cos(simplify(z))
    case Acos(z)        => Acos(simplify(z))
    case Tan(z)         => Tan(simplify(z))
    case Atan(z)        => Atan(simplify(z))
    case Sinh(z)        => Sinh(simplify(z))
    case Asinh(z)       => Asinh(simplify(z))
    case Cosh(z)        => Cosh(simplify(z))
    case Acosh(z)       => Acosh(simplify(z))
    case Tanh(z)        => Tanh(simplify(z))
    case Atanh(z)       => Atanh(simplify(z))
    case z              => z
  }
  
  object BigDecimal {
    def unapply(decimal: BigDecimal): Option[Double] = Some(decimal.toDouble)
  }
  
  private def simplifications(function: ComplexFunction) : ComplexFunction = function match {
    case Add(Constant(a), Constant(b))            => a + b
    case Add(Constant(Zero), z)                   => simplify(z)
    case Add(z, Constant(Zero))                   => simplify(z)
    case Subtract(Constant(a), Constant(b))       => a - b
    case Subtract(Constant(Zero), z)              => simplify(-z)
    case Subtract(z, Constant(Zero))              => simplify(z)
    case Multiply(Constant(a), Constant(b))       => a * b
    case Multiply(Constant(Zero), _)              => 0
    case Multiply(_, Constant(Zero))              => 0
    case Multiply(Constant(One), z)               => simplify(z)
    case Multiply(z, Constant(One))               => simplify(z)
    case Divide(Constant(a), Constant(b))         => a / b
    case Divide(z, Constant(One))                 => simplify(z)
    case Power(Constant(a), Constant(b))          => a ~^ b
    case Power(Constant(One), _)                  => 1
    case Power(Power(z, Constant(a)),Constant(b)) => simplify(z) ~^ (a * b)
    case Power(z, Constant(One))                  => simplify(z)
    case UnaryMinus(Constant(z))                  => -z
    case UnaryMinus(UnaryMinus(z))                => simplify(z)
    case UnaryMinus(Subtract(a, b))               => Subtract(simplify(b), simplify(a))
    case Conj(Constant(z))                        => z.conj
    case Abs(Constant(z))                         => z.abs
    case Arg(Constant(z))                         => z.arg
    case Sqrt(Constant(z))                        => z.sqrt
    case Sqrt(Power(z, Constant(Complex(2, 0))))  => simplify(z)
    case Im(Constant(z))                          => z.im
    case Re(Constant(z))                          => z.re
    case Log(Constant(E))                         => 1
    case Log(Power(Constant(E), z))               => simplify(z)
    case Sin(Asin(z))                             => simplify(z)
    case Cos(Acos(z))                             => simplify(z)
    case Tan(Atan(z))                             => simplify(z)
    case Sinh(Asinh(z))                           => simplify(z)
    case Cosh(Acosh(z))                           => simplify(z)
    case Tanh(Atanh(z))                           => simplify(z)
    case z                                        => z
  }
}