package net.totietje.complex

import Complex._
import ComplexFunction._

import scala.language.implicitConversions

/** This represents a function of complex variables. It can be evaluated using a map of the variables and their
  * values. The function will then be evaluated, substituting in each variable's respective value, returning
  * a complex number as its output.
  *
  * @see [[net.totietje.complex.Complex Complex]]
  */
sealed trait ComplexFunction {
  
  /** Evaluates the complex function, substituting in the values for each variable as given in the map.
    *
    * If the function contains a variable whose value is not given by the variable map, it would throw a
    * [[net.totietje.complex.VariableException VariableException]].
    *
    * If no map is provided, by default it will be empty, so if it encounters a variable it will throw a
    * [[net.totietje.complex.VariableException VariableException]].
    * @param in
    *          The variable map, `null` by default
    * @return
    *         The evaluated function
    * @throws net.totietje.complex.VariableException
    *                           If the function contains an undefined variable
    */
  def apply(in: Map[String, Complex] = Map()) : Complex
  
  /** Adds this function to another one.
    *
    * This creates a compound function which, when evaluated, returns the result of evaluating
    * this function added to the result of evaluating the other.
    * @see [[net.totietje.complex.ComplexFunction.Add Add]]
    * @param that
    *             The function to be added to this one
    * @return
    *         `Add(this, that)`
    */
  def +(that: ComplexFunction) = Add(this, that)
  
  /** Subtracts another function from this one.
    *
    * This creates a compound function which, when evaluated, returns the result of evaluating
    * this function minus the result of evaluating the other.
    * @see [[net.totietje.complex.ComplexFunction.Subtract Subtract]]
    * @param that
    *             The function to be subtracted from this one
    * @return
    *         `Subtract(this, that)`
    */
  def -(that: ComplexFunction) = Subtract(this, that)
  
  /** Multiplies this function by another one.
    *
    * This creates a compound function which, when evaluated, returns the result of evaluating
    * this function multiplied by the result of evaluating the other.
    * @see [[net.totietje.complex.ComplexFunction.Multiply Multiply]]
    * @param that
    *             The function to be multiplied by this one
    * @return
    *         `Multiply(this, that)`
    */
  def *(that: ComplexFunction) = Multiply(this, that)
  
  /** Divides this function by another one.
    *
    * This creates a compound function which, when evaluated, returns the result of evaluating
    * this function divided by the result of evaluating the other.
    * @see [[net.totietje.complex.ComplexFunction.Divide Divide]]
    * @param that
    *             The function to divide this one by
    * @return
    *         `Divide(this, that)`
    */
  def /(that: ComplexFunction) = Divide(this, that)
  
  /** Raises this function to the power of another one
    *
    * This creates a compound function which, when evaluated, returns the result of evaluating
    * this function to the power of the result of evaluating the other.
    * @see [[net.totietje.complex.ComplexFunction.Power Power]]
    * @param that
    *             The exponent
    * @return
    *         `Power(this, that)`
    */
  def ~^(that: ComplexFunction) = Power(this, that)
  
  /** Negates this function.
    *
    * This creates a function which, when evaluated, returns the negative of the result
    * of evaluating this function.
    * @return
    *         `UnaryMinus(this)`
    */
  def unary_- : ComplexFunction = UnaryMinus(this)
  
  /** Returns a string representation of this function.
    * @return
    *         A string representation of this function
    */
  override def toString: String = ComplexFunction.toString(this)
}

/** A companion object for the `ComplexFunction` trait.
  *
  * This contains all the subclasses of the sealed `ComplexFunction` class, and
  * implicit conversions.
  */
object ComplexFunction {
  
  /** Implicitly wraps the input as a [[net.totietje.complex.ComplexFunction.Constant Constant]].
    * @param v
    *          The input to be converted to a `Constant`.
    * @return
    *         A `Constant` which, when evaluated, will return the input.
    */
  implicit def toConstant(v: Complex): Constant = Constant(v)
  
  /** Implicitly wraps the input as a [[net.totietje.complex.ComplexFunction.Constant Constant]].
    * @param v
    *          The input to be converted to a `Constant`.
    * @return
    *         A `Constant` which, when evaluated, will return the input as a [[net.totietje.complex.Complex Complex]].
    */
  implicit def toConstant(v: Double): Constant = Constant(v)
  
  /** Implicitly wraps the input as a [[net.totietje.complex.ComplexFunction.Constant Constant]].
    * @param v
    *          The input to be converted to a `Constant`.
    * @return
    *         A `Constant` which, when evaluated, will return the input as a [[net.totietje.complex.Complex Complex]].
    */
  implicit def toConstant(v: Int): Constant = Constant(v)
  
  /** A variable which, when evaluated, will return its respective function in the variable map.
    *
    * If, when evaluated, the variable's string representation is not found in the map, it will throw a
    * [[net.totietje.complex.VariableException VariableException]].
    * @param variable
    *                 The string representation of the variable
    */
  case class Variable(variable: String) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = in.get(variable) match {
      case Some(value) => value
      case None => throw VariableException(s"$variable is undefined")
    }
  }
  
  /** A constant which, when evaluated, always returns the same value.
    * @param z
    *          The value that this constant represents
    */
  case class Constant(z: Complex) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z
  }
  
  /** A function which adds together two other functions.
    *
    * When evaluated, the result will be the result of evaluating one function added to the result
    * of evaluating the other.
    * @param a
    *          One of the two functions to be added
    * @param b
    *          The other of the two functions to be added
    */
  case class Add(a: ComplexFunction, b: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = a(in) + b(in)
  }
  
  /** A function which subtracts one function from another.
    *
    * When evaluated, the result will be the result of evaluating one function subtracted from the result
    * of evaluating the other.
    * @param a
    *          The minuend
    * @param b
    *          The subtrahend
    */
  case class Subtract(a: ComplexFunction, b: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = a(in) - b(in)
  }
  
  /** A function which multiplies one function by another.
    *
    * When evaluated, the result will be the result of evaluating one function multiplied by the result
    * of evaluating the other.
    * @param a
    *          The first function to be multiplied
    * @param b
    *          The second function to be multiplied
    */
  case class Multiply(a: ComplexFunction, b: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = a(in) * b(in)
  }
  
  /** A function which divides one function by another.
    *
    * When evaluated, the result will be the result of evaluating one function divided by the result
    * of evaluating the other.
    * @param a
    *          The dividend
    * @param b
    *          The divisor
    */
  case class Divide(a: ComplexFunction, b: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = a(in) / b(in)
  }
  
  /** A function which raises one function to the power of another.
    *
    * When evaluated, the result will be the result of evaluating one function to the power of the result
    * of evaluating the other.
    * @param a
    *          The base
    * @param b
    *          The exponent
    */
  case class Power(a: ComplexFunction, b: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = a(in) ~^ b(in)
  }
  
  /** A function which is the negative of another.
    *
    * When evaluated, the result will be the negative of the result of evaluating the other function.
    * @param z
    *          The function to be negated
    */
  case class UnaryMinus(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = -z(in)
  }
  
  /** A function which is the conjugate of another.
    *
    * When evaluated, the result will be the conjugate of the result of evaluating the other function, that is,
    * it will have the same real part but a negated imaginary part.
    * @param z
    *          The function of which this is the conjugate
    */
  case class Conj(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).conj
  }
  
  /** A function which is the absolute of another.
    *
    * When evaluated, the result will be the absolute of the result of evaluating the other function. Since the
    * absolute only returns real numbers, the result will have imaginary part 0.
    * @param z
    *          The function of which this is the absolute
    */
  case class Abs(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).abs
  }
  
  /** A function which is the argument of another.
    *
    * When evaluated, the result will be the argument of the result of evaluating the other function. Since the
    * argument only returns real numbers, the result will have imaginary part 0.
    * @param z
    *          The function of which this is the argument
    */
  case class Arg(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).arg
  }
  
  /** A function which is the square root of another.
    *
    * When evaluated, the result will be the square root of the result of evaluating the other function.
    * @param z
    *          The function of which this is the square root
    */
  case class Sqrt(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).sqrt
  }
  
  /** A function which is the real part of another.
    *
    * When evaluated, the result will be the real part of the result of evaluating the other function. The result
    * will have imaginary part 0.
    * @param z
    *          The function of which this is the real part
    */
  case class Re(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).re
  }
  
  /** A function which is the imaginary part of another.
    *
    * When evaluated, the result will be the imaginary part of the result of evaluating the other function. The
    * result will have imaginary part 0, and its real part will be equal to the imaginary part of the other function.
    * @param z
    *          The function of which this is the imaginary part
    */
  case class Im(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).im
  }
  
  /** A function which is the natural logarithm of another.
    *
    * When evaluated, the result will be the natural logarithm of the result of evaluating the other function.
    * @param z
    *          The function of which this is the natural logarithm
    */
  case class Log(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).log
  }
  
  /** A function which is the sine of another.
    *
    * When evaluated, the result will be the sine of the result of evaluating the other function.
    * @param z
    *          The function of which this is the sine
    */
  case class Sin(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).sin
  }
  
  /** A function which is the inverse sine of another.
    *
    * When evaluated, the result will be the inverse sine of the result of evaluating the other function.
    * @param z
    *          The function of which this is the inverse sine
    */
  case class Asin(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).asin
  }
  
  /** A function which is the cosine of another.
    *
    * When evaluated, the result will be the cosine of the result of evaluating the other function.
    * @param z
    *          The function of which this is the cosine
    */
  case class Cos(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).cos
  }
  
  /** A function which is the inverse cosine of another.
    *
    * When evaluated, the result will be the inverse cosine of the result of evaluating the other function.
    * @param z
    *          The function of which this is the inverse cosine
    */
  case class Acos(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).acos
  }
  
  /** A function which is the tangent of another.
    *
    * When evaluated, the result will be the tangent of the result of evaluating the other function.
    * @param z
    *          The function of which this is the tangent
    */
  case class Tan(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).tan
  }
  
  /** A function which is the inverse tangent of another.
    *
    * When evaluated, the result will be the inverse tangent of the result of evaluating the other function.
    * @param z
    *          The function of which this is the inverse tangent
    */
  case class Atan(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).atan
  }
  
  /** A function which is the hyperbolic sine of another.
    *
    * When evaluated, the result will be the hyperbolic sine of the result of evaluating the other function.
    * @param z
    *          The function of which this is the hyperbolic sine
    */
  case class Sinh(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).sinh
  }
  
  /** A function which is the inverse hyperbolic sine of another.
    *
    * When evaluated, the result will be the inverse hyperbolic sine of the result of evaluating the other function.
    * @param z
    *          The function of which this is the inverse hyperbolic sine
    */
  case class Asinh(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).asinh
  }
  
  /** A function which is the hyperbolic cosine of another.
    *
    * When evaluated, the result will be the hyperbolic cosine of the result of evaluating the other function.
    * @param z
    *          The function of which this is the hyperbolic cosine
    */
  case class Cosh(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).cosh
  }
  
  /** A function which is the inverse hyperbolic cosine of another.
    *
    * When evaluated, the result will be the inverse hyperbolic cosine of the result of evaluating the other function.
    * @param z
    *          The function of which this is the inverse hyperbolic cosine
    */
  case class Acosh(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).acosh
  }
  
  /** A function which is the hyperbolic tangent of another.
    *
    * When evaluated, the result will be the hyperbolic tangent of the result of evaluating the other function.
    * @param z
    *          The function of which this is the hyperbolic tangent
    */
  case class Tanh(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).tanh
  }
  
  /** A function which is the inverse hyperbolic tangent of another.
    *
    * When evaluated, the result will be the inverse hyperbolic tangent of the result of evaluating the other function.
    * @param z
    *          The function of which this is the inverse hyperbolic tangent
    */
  case class Atanh(z: ComplexFunction) extends ComplexFunction {
    def apply(in: Map[String, Complex]): Complex = z(in).atanh
  }
  
  /** Determines whether the given function contains only the provided variables, and not any other.
    *
    * For this to return true, it does not need to contain all the provided variables, as long as it does not
    * contain any variables other than those provided.
    *
    * If this method returns true, it means that the evaluating the function will not result in a
    * [[net.totietje.complex.VariableException VariableException]] being thrown if those variables are provided
    * with values in the input map.
    * @param function
    *                 A `ComplexFunction`
    * @param vars
    *             An array of strings containing the allowed variables
    * @return
    *         `true` if the function does not contain any variables other than those provided
    */
  def evaluableBy(function: ComplexFunction)(implicit vars: Array[String]): Boolean = function match {
    case Variable(name) => vars.contains(name)
    case Constant(_)    => true
    case Add(a, b)      => evaluableBy(a) && evaluableBy(b)
    case Subtract(a, b) => evaluableBy(a) && evaluableBy(b)
    case Multiply(a, b) => evaluableBy(a) && evaluableBy(b)
    case Divide(a, b)   => evaluableBy(a) && evaluableBy(b)
    case Power(a, b)    => evaluableBy(a) && evaluableBy(b)
    case UnaryMinus(z)  => evaluableBy(z)
    case Conj(z)        => evaluableBy(z)
    case Abs(z)         => evaluableBy(z)
    case Arg(z)         => evaluableBy(z)
    case Im(z)          => evaluableBy(z)
    case Re(z)          => evaluableBy(z)
    case Sqrt(z)        => evaluableBy(z)
    case Log(z)         => evaluableBy(z)
    case Sin(z)         => evaluableBy(z)
    case Asin(z)        => evaluableBy(z)
    case Cos(z)         => evaluableBy(z)
    case Acos(z)        => evaluableBy(z)
    case Tan(z)         => evaluableBy(z)
    case Atan(z)        => evaluableBy(z)
    case Sinh(z)        => evaluableBy(z)
    case Asinh(z)       => evaluableBy(z)
    case Cosh(z)        => evaluableBy(z)
    case Acosh(z)       => evaluableBy(z)
    case Tanh(z)        => evaluableBy(z)
    case Atanh(z)       => evaluableBy(z)
  }
  
  /** Differentiates this function with respect to a given variable.
    *
    * This returns a new function which is the derivative of the input.
    *
    * The functions `Conj`, `Abs`, `Arg`, `Im` and `Re` are not differentiable. If they are part of the function,
    * then, on evaluating the derivative, the function will only output NaN.
    * @param function
    *                 The function to differentiate
    * @param respect
    *                The variable to differentiate with respect to, `x` by default
    * @return
    *         The derivative of the function
    */
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
  
  /** Simplifies the input function.
    *
    * The new function will be equivalent, producing the same output for the same inputs. However, it will be simpler,
    * eliminating redundant calculations such as multiplications by 1, or the sine of the inverse sine. After
    * simplifying a function, evaluating it will likely be faster.
    * @param function
    *                 The function to simplify
    * @return
    *         The simplified function
    */
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
}