package net.totietje.complex

import net.totietje.complex.Complex._
import net.totietje.complex.ComplexFunction._

import scala.language.implicitConversions

/** This represents a function of complex variables. It can be evaluated using a map of the variables and their
  * values. The function will then be evaluated, substituting in each variable's respective value, returning
  * a complex number as its output.
  *
  * @see [[net.totietje.complex.Complex Complex]]
  */
abstract class ComplexFunction {
  /** Evaluates the complex function, substituting in the values for each variable as given in the map.
    *
    * If the function contains a variable whose value is not given by the variable map, it will throw a
    * [[net.totietje.complex.VariableException VariableException]].
    *
    * If no map is provided, it will be empty by default. Thus, if the function contains a variable, it will throw a
    * [[net.totietje.complex.VariableException VariableException]].
    * @param in
    *          The variable map
    * @return
    *         The evaluated function
    * @throws net.totietje.complex.VariableException
    *                           If the function contains a variable undefined by the map
    */
  def apply(in: Map[String, Complex]) : Complex
  
  /** Evaluates the complex function, substituting in the values for each variable as given in the input map in the
    * form of tuple key-value pairs.
    *
    * If the function contains a variable whose value is not given by input variables, it will throw a
    * [[net.totietje.complex.VariableException VariableException]].
    *
    * If nothing is provided, it will be empty by default. Thus, if the function contains a variable, it will throw a
    * [[net.totietje.complex.VariableException VariableException]].
    * @param in
    *          The variables to substitute in
    * @param default
    *                 The default variable map, empty by default. If a variable is defined both by `in` and by
    *                 `defaults`, the value given in `in` takes priority.
    * @return
    *         The evaluated function
    * @throws net.totietje.complex.VariableException
    *                           If the function contains a variable whose value is not given
    */
  def apply(in: (String, Complex)*)(implicit default: Map[String, Complex] = Map()): Complex = {
    apply(default ++ Map(in:_*))
  }
  
  /** Evaluates this complex function, substituting the value for the given variable with the given value.
    *
    * If the function contains a variable other than the one given, it will throw a
    * [[net.totietje.complex.VariableException VariableException]].
    * @param value
    *              The value to substitute in for the variable
    * @param variable
    *                 The variable whose value is to be substituted in
    * @return
    *         The evaluated function
    * @throws net.totietje.complex.VariableException
    *                           If the function contains a variable other than the one given
    */
  def apply(value: Complex)(implicit variable: String): Complex = {
    apply(Map(variable -> value))
  }
  
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
  override def toString: String
  
  /** Differentiates this function.
    *
    * If this function is not differentiable, then it should contain NaN, such that hasNaN returns true.
    * @param respect
    *                The variable to differentiate with respect to.
    */
  def derivative(implicit respect: String): ComplexFunction
  
  /** Simplifies this function.
    *
    * The returned function should be equivalent to the old function for all inputs. Simplified functions
    * are likely to be faster, as they remove redundant calculations.
    * @return
    *         This function simplified
    */
  def simplify: ComplexFunction
  
  /** The input arguments of this `ComplexFunction`.
    * @return
    *         The arguments
    */
  def arguments: Seq[ComplexFunction]
  
  /** Determines whether this function contains only the provided variables, and not any other.
    *
    * For this to return true, it does not need to contain all the provided variables, but it must not
    * contain any variables other than those provided.
    *
    * If this method returns true, it means that the evaluating the function will not result in a
    * [[net.totietje.complex.VariableException VariableException]] being thrown if those variables are provided
    * with values in the input map.
    * @param by
    *             The allowed variables. The implicit conversion `toSet` allows the user to pass in
    *             only one `String`, which will be converted to a `Set[String]`.
    * @return
    *         `true` if the function does not contain any variables other than those provided
    */
  def evaluable(implicit by: Set[String]): Boolean = arguments.forall(_.evaluable)
  
  /** Returns all the variables which this function contains.
    *
    * All of these variables must be provided in order to evaluate this function.
    * @return
    *         All the variables this function contains.
    */
  def variables: Set[String] = arguments.flatMap(_.variables).toSet
  
  /** Whether this function contains the constant NaN.
    * @return
    *         Whether this function contains the constant NaN.
    */
  def hasNaN: Boolean = arguments.exists(_.hasNaN)
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
  
  /** Implicitly converts a `String` into a `Set[String]`
    *
    * @param str
    *            The `String` to convert
    * @return
    *         A `Set[String]` containing only the input
    */
  implicit def toSet(implicit str: String): Set[String] = Set(str)
  
  /** Represents a `ComplexFunction` with no arguments.
    */
  abstract class NullaryFunction extends ComplexFunction {
    override def arguments: Seq[ComplexFunction] = Seq()
    override def simplify: ComplexFunction = this
  }
  
  /** The companion object for the `NullaryFunction` class, containing only an unapply method.
    */
  object NullaryFunction {
    /** Returns `Some()`
      * @param function
      *            A `NullaryFunction`, ignored
      * @return
      *         `Some()`
      */
    def unapply(function: NullaryFunction): Option[Unit] = Some(())
  }
  
  /** Represents a `ComplexFunction` with one argument.
    * @param function
    *                 The function this represents
    */
  abstract class UnaryFunction(function: Complex => Complex) extends ComplexFunction {
    /** The sole argument.
     */
    val argument: ComplexFunction
    
    override def apply(in: Map[String, Complex]): Complex = function(argument(in))
    
    override def arguments: Seq[ComplexFunction] = Seq(argument)
  }
  
  /** The companion object for the `UnaryFunction` class, containing only an unapply method.
    */
  object UnaryFunction {
    
    /** Returns the function's argument wrapped in a `Some()`.
      * @param function
      *                 The function to unapply
      * @return
      *         `Some(function.argument)`
      */
    def unapply(function: UnaryFunction): Option[ComplexFunction] = Some(function.argument)
  }
  
  /** Represents a `ComplexFunction` with two arguments.
    * @param function
    *                 The function this represents
    */
  abstract class BinaryFunction(function: (Complex, Complex) => Complex) extends ComplexFunction {
    
    /** The first argument
      */
    val left: ComplexFunction
    
    /** The second argument
      */
    val right: ComplexFunction
    
    override def apply(in: Map[String, Complex]): Complex = function(left(in), right(in))
  
    override def arguments: Seq[ComplexFunction] = Seq(left, right)
  }
  
  /** The companion object for the `BinaryFunction` class, containing only an unapply method.
    */
  object BinaryFunction {
    /** Returns the function's arguments wrapped in a `Some()`.
      * @param function
      *                 The function to unapply
      * @return
      *         `Some(function.left, function.right)`
      */
    def unapply(function: BinaryFunction): Option[(ComplexFunction, ComplexFunction)] = {
      Some(function.left, function.right)
    }
  }
  
  /** Represents a `ComplexFunction` with three arguments.
    * @param function
    *                 The function this represents
    */
  abstract class TernaryFunction(function: (Complex, Complex, Complex) => Complex) extends ComplexFunction {
    /** The first argument
      */
    val first: ComplexFunction
  
    /** The second argument
      */
    val second: ComplexFunction
  
    /** The third argument
      */
    val third: ComplexFunction
  
    override def apply(in: Map[String, Complex]): Complex = function(first(in), second(in), third(in))
    
    override def arguments: Seq[ComplexFunction] = Seq(first, second, third)
  }
  
  /** The companion object for the `TernaryFunction` class, containing only an unapply method.
    */
  object TernaryFunction {
  
    /** Returns the function's arguments wrapped in a `Some()`.
      * @param function
      *                 The function to unapply
      * @return
      *         `Some(function.first, function.second, function.third)`
      */
    def unapply(function: TernaryFunction): Option[(ComplexFunction, ComplexFunction, ComplexFunction)] = {
      Some(function.first, function.second, function.third)
    }
  }
  
  /** Represents a `ComplexFunction` with a variable number of arguments.
    * @param function
    *                 The function this represents
    */
  abstract class VariadicFunction(function: (Complex*) => Complex) extends ComplexFunction {
    override def apply(in: Map[String, Complex]): Complex = {
      function(arguments.map(_(in)):_*)
    }
  }
  
  /** The companion object for the `VariadicFunction` class, containing only an unapply method.
    */
  object VariadicFunction {
    /** Returns the function's arguments wrapped in a `Some()`.
      * @param function
      *                 The function to unapply
      * @return
      *         `Some(function.arguments)`
      */
    def unapply(function: VariadicFunction): Option[Seq[ComplexFunction]] = Some(function.arguments)
  }
  
  /** A variable which, when evaluated, will return its respective function in the variable map.
    *
    * If, when evaluated, the variable's string representation is not found in the map, it will throw a
    * [[net.totietje.complex.VariableException VariableException]].
    * @param variable
    *                 The string representation of the variable
    */
  case class Variable(variable: String) extends NullaryFunction {
    def apply(in: Map[String, Complex]): Complex = in.get(variable) match {
      case Some(value) => value
      case None => throw VariableException(s"$variable is undefined")
    }
    
    override def derivative(implicit respect: String): ComplexFunction = {
      if (respect == variable) 1 else 0
    }
    
    override def toString: String = variable
  
    override def evaluable(implicit by: Set[String]): Boolean = by(variable)
  
    override def variables: Set[String] = Set(variable)
  }
  
  /** A constant which, when evaluated, always returns the same value.
    * @param value
    *          The value that this constant represents
    */
  case class Constant(value: Complex) extends NullaryFunction {
    def apply(in: Map[String, Complex]): Complex = value
    
    override def derivative(implicit respect: String): ComplexFunction = 0
    
    override def toString: String = value match {
      case E    => "e"
      case Pi   => "π"
      case Tau  => "(2 * π)"
      case _    => value.toString
    }
    
    override def hasNaN: Boolean = value.hasNaNPart
  }
  
  /** A function which adds together two other functions.
    *
    * When evaluated, the result will be the result of evaluating one function added to the result
    * of evaluating the other.
    * @param left
    *          One of the two functions to be added
    * @param right
    *          The other of the two functions to be added
    */
  case class Add(left: ComplexFunction, right: ComplexFunction) extends BinaryFunction(_ + _) {
    override def derivative(implicit respect: String): ComplexFunction = {
      left.derivative + right.derivative
    }
    
    override def simplify: ComplexFunction = (left.simplify, right.simplify) match {
      case (Constant(a), Constant(b)) => a + b
      case (Constant(Zero), z)        => z
      case (z, Constant(Zero))        => z
      case (a, b) if a == b           => 2 * a
      case (a, b) if a == -b          => 0
      case (a, b) if -a == b          => 0
      case (a, b)                     => a + b
    }
    
    override def toString: String = s"($left + $right)"
  }
  
  /** A function which subtracts one function from another.
    *
    * When evaluated, the result will be the result of evaluating one function subtracted from the result
    * of evaluating the other.
    * @param left
    *          The minuend
    * @param right
    *          The subtrahend
    */
  case class Subtract(left: ComplexFunction, right: ComplexFunction) extends BinaryFunction(_ - _) {
    override def derivative(implicit respect: String): ComplexFunction = {
      left.derivative - right.derivative
    }
    
    override def simplify: ComplexFunction = (left.simplify, right.simplify) match {
      case (Constant(x), Constant(y)) => x - y
      case (Constant(Zero), z)        => (-z).simplify
      case (z, Constant(Zero))        => z
      case (x, y) if x == y           => 0
      case (x, y) if x == -y          => 2 * x
      case (x, y) if -x == y          => 2 * x
      case (x, UnaryMinus(y))         => x + y
      case (a, b)                     => a - b
    }
    
    override def toString: String = s"($left - $right)"
  }
  
  /** A function which multiplies one function by another.
    *
    * When evaluated, the result will be the result of evaluating one function multiplied by the result
    * of evaluating the other.
    * @param left
    *          The first function to be multiplied
    * @param right
    *          The second function to be multiplied
    */
  case class Multiply(left: ComplexFunction, right: ComplexFunction) extends BinaryFunction(_ * _) {
    override def derivative(implicit respect: String): ComplexFunction = {
      left * right.derivative + right * left.derivative
    }
    
    override def simplify: ComplexFunction = (left.simplify, right.simplify) match {
      case (Constant(x), Constant(y))     => x * y
      case (Constant(Zero), _)            => 0
      case (_, Constant(Zero))            => 0
      case (Constant(One), z)             => z
      case (z, Constant(One))             => z
      case (UnaryMinus(a), UnaryMinus(b)) => a * b
      case (a, b)                         => a * b
    }
    
    override def toString: String = s"($left * $right)"
  }
  
  /** A function which divides one function by another.
    *
    * When evaluated, the result will be the result of evaluating one function divided by the result
    * of evaluating the other.
    * @param left
    *          The dividend
    * @param right
    *          The divisor
    */
  case class Divide(left: ComplexFunction, right: ComplexFunction) extends BinaryFunction(_ / _) {
    override def derivative(implicit respect: String): ComplexFunction = {
      right * left.derivative - left * right.derivative / (right ~^ 2)
    }
    
    override def simplify: ComplexFunction = (left.simplify, right.simplify) match {
      case (Constant(a), Constant(b)) => a / b
      case (z, Constant(One))         => z
      case (a, b)                     => a / b
    }
    
    override def toString: String = s"($left / $right)"
  }
  
  /** A function which raises one function to the power of another.
    *
    * When evaluated, the result will be the result of evaluating one function to the power of the result
    * of evaluating the other.
    * @param left
    *          The base
    * @param right
    *          The exponent
    */
  case class Power(left: ComplexFunction, right: ComplexFunction) extends BinaryFunction(_ ~^ _) {
    override def derivative(implicit respect: String): ComplexFunction = {
      (left ~^ right) * (left.derivative * right / left + right.derivative * Log(left))
    }
    
    override def simplify: ComplexFunction = (left.simplify, right.simplify) match {
      case (Constant(a), Constant(b))            => a ~^ b
      case (Constant(One), _)                    => 1
      case (Power(z, Constant(a)), Constant(b))  => z ~^ (a * b)
      case (z, Constant(One))                    => z
      case (Sqrt(z), Constant(Complex(2, 0)))    => z
      case (a, b)                                => a ~^ b
    }
    
    override def toString: String = s"($left ^ $right)"
  }
  
  /** A function which is the negative of another.
    *
    * When evaluated, the result will be the negative of the result of evaluating the other function.
    * @param argument
    *          The function to be negated
    */
  case class UnaryMinus(argument: ComplexFunction) extends UnaryFunction(-_) {
    override def derivative(implicit respect: String): ComplexFunction = -argument.derivative
    
    override def simplify: ComplexFunction = argument.simplify match {
      case Constant(x)     => -x
      case UnaryMinus(x)   => x
      case Subtract(a, b)  => b - a
      case other           => -other
    }
    
    override def toString: String = s"(-$argument)"
  }
  
  /** A function which is the conjugate of another.
    *
    * When evaluated, the result will be the conjugate of the result of evaluating the other function, that is,
    * it will have the same real part but a negated imaginary part.
    * @param argument
    *          The function of which this is the conjugate
    */
  case class Conj(argument: ComplexFunction) extends UnaryFunction(_.conj) {
    override def derivative(implicit respect: String): ComplexFunction = NaN
    
    override def simplify: ComplexFunction = argument.simplify match {
      case Arg(x)       => Arg(x)
      case Abs(x)       => Abs(x)
      case Im(x)        => Im(x)
      case Re(x)        => Re(x)
      case Conj(x)      => Conj(x)
      case Constant(x)  => x.conj
      case other        => Conj(other)
    }
    
    override def toString: String = s"conj($argument)"
  }
  
  /** A function which is the absolute of another.
    *
    * When evaluated, the result will be the absolute of the result of evaluating the other function. Since the
    * absolute only returns real numbers, the result will have imaginary part 0.
    * @param argument
    *          The function of which this is the absolute
    */
  case class Abs(argument: ComplexFunction) extends UnaryFunction(_.abs) {
    override def derivative(implicit respect: String): ComplexFunction = NaN
    
    override def simplify: ComplexFunction = argument.simplify match {
      case Conj(x) => Abs(x)
      case Abs(x)  => Abs(x)
      case other   => Abs(other)
    }
    
    override def toString: String = s"abs($argument)"
  }
  
  /** A function which is the argument of another.
    *
    * When evaluated, the result will be the argument of the result of evaluating the other function. Since the
    * argument only returns real numbers, the result will have imaginary part 0.
    * @param argument
    *          The function of which this is the argument
    */
  case class Arg(argument: ComplexFunction) extends UnaryFunction(_.arg) {
    override def derivative(implicit respect: String): ComplexFunction = NaN
    
    override def simplify: ComplexFunction = Arg(argument.simplify)
    
    override def toString: String = s"arg($argument)"
  }
  
  /** A function which is the square root of another.
    *
    * When evaluated, the result will be the square root of the result of evaluating the other function.
    * @param argument
    *          The function of which this is the square root
    */
  case class Sqrt(argument: ComplexFunction) extends UnaryFunction(_.sqrt) {
    override def derivative(implicit respect: String): ComplexFunction = {
      0.5 * (argument ~^ -0.5) * argument.derivative
    }
    
    override def simplify: ComplexFunction = Sqrt(argument.simplify)
    
    override def toString: String = s"√($argument)"
  }
  
  /** A function which is the real part of another.
    *
    * When evaluated, the result will be the real part of the result of evaluating the other function. The result
    * will have imaginary part 0.
    * @param argument
    *          The function of which this is the real part
    */
  case class Re(argument: ComplexFunction) extends UnaryFunction(_.re) {
    override def derivative(implicit respect: String): ComplexFunction = NaN
    
    override def simplify: ComplexFunction = argument.simplify match {
      case Conj(x)      => Re(x)
      case Im(x)        => Im(x)
      case Re(x)        => Re(x)
      case Arg(x)       => Arg(x)
      case Abs(x)       => Abs(x)
      case Constant(x)  => x.re
      case x            => Re(x)
    }
    
    override def toString: String = s"re($argument)"
  }
  
  /** A function which is the imaginary part of another.
    *
    * When evaluated, the result will be the imaginary part of the result of evaluating the other function. The
    * result will have imaginary part 0, and its real part will be equal to the imaginary part of the other function.
    * @param argument
    *          The function of which this is the imaginary part
    */
  case class Im(argument: ComplexFunction) extends UnaryFunction(_.im) {
    override def derivative(implicit respect: String): ComplexFunction = NaN
    
    override def simplify: ComplexFunction = argument.simplify match {
      case Conj(x)      => -Im(x)
      case Im(_)        => 0
      case Re(_)        => 0
      case Arg(_)       => 0
      case Abs(_)       => 0
      case Constant(x)  => x.im
      case x            => Im(x)
    }
    
    override def toString: String = s"im($argument)"
  }
  
  /** A function which is the natural logarithm of another.
    *
    * When evaluated, the result will be the natural logarithm of the result of evaluating the other function.
    * @param argument
    *          The function of which this is the natural logarithm
    */
  case class Log(argument: ComplexFunction) extends UnaryFunction(_.log) {
    override def derivative(implicit respect: String): ComplexFunction = {
      argument.derivative / argument
    }
    
    override def simplify: ComplexFunction = argument.simplify match {
      case Constant(E)            => 1
      case Power(Constant(E), x)  => x
      case x                      => Log(x)
    }
    
    override def toString: String = s"log($argument)"
  }
  
  /** A function which is the sine of another.
    *
    * When evaluated, the result will be the sine of the result of evaluating the other function.
    * @param argument
    *          The function of which this is the sine
    */
  case class Sin(argument: ComplexFunction) extends UnaryFunction(_.sin) {
    override def derivative(implicit respect: String): ComplexFunction = Cos(argument) * argument.derivative
    
    override def simplify: ComplexFunction = argument.simplify match {
      case Asin(x)  => x
      case x        => Sin(x)
    }
  
    override def toString: String = s"sin($argument)"
  }
  
  /** A function which is the inverse sine of another.
    *
    * When evaluated, the result will be the inverse sine of the result of evaluating the other function.
    * @param argument
    *          The function of which this is the inverse sine
    */
  case class Asin(argument: ComplexFunction) extends UnaryFunction(_.asin) {
    override def derivative(implicit respect: String): ComplexFunction = {
      argument.derivative / Sqrt(1 - (argument ~^ 2))
    }
    
    override def simplify: ComplexFunction = Asin(argument.simplify)
  
    override def toString: String = s"asin($argument)"
  }
  
  /** A function which is the cosine of another.
    *
    * When evaluated, the result will be the cosine of the result of evaluating the other function.
    * @param argument
    *          The function of which this is the cosine
    */
  case class Cos(argument: ComplexFunction) extends UnaryFunction(_.cos) {
    override def derivative(implicit respect: String): ComplexFunction = -(Sin(argument) * argument.derivative)
    
    override def simplify: ComplexFunction = argument.simplify match {
      case Acos(x)  => x
      case x        => Cos(x)
    }
  
    override def toString: String = s"cos($argument)"
  }
  
  /** A function which is the inverse cosine of another.
    *
    * When evaluated, the result will be the inverse cosine of the result of evaluating the other function.
    * @param argument
    *          The function of which this is the inverse cosine
    */
  case class Acos(argument: ComplexFunction) extends UnaryFunction(_.acos) {
    override def derivative(implicit respect: String): ComplexFunction = -(1 / Sqrt(1 - (argument ~^ 2)))
    
    override def simplify: ComplexFunction = Acos(argument.simplify)
  
    override def toString: String = s"acos($argument)"
  }
  
  /** A function which is the tangent of another.
    *
    * When evaluated, the result will be the tangent of the result of evaluating the other function.
    * @param argument
    *          The function of which this is the tangent
    */
  case class Tan(argument: ComplexFunction) extends UnaryFunction(_.tan) {
    override def derivative(implicit respect: String): ComplexFunction = argument.derivative / (Cos(argument) ~^ 2)
    
    override def simplify: ComplexFunction = argument.simplify match {
      case Atan(x)  =>  x
      case x        => Tan(x)
    }
  
    override def toString: String = s"tan($argument)"
  }
  
  /** A function which is the inverse tangent of another.
    *
    * When evaluated, the result will be the inverse tangent of the result of evaluating the other function.
    * @param argument
    *          The function of which this is the inverse tangent
    */
  case class Atan(argument: ComplexFunction) extends UnaryFunction(_.atan) {
    override def derivative(implicit respect: String): ComplexFunction = argument.derivative / (1 + (argument ~^ 2))
    
    override def simplify: ComplexFunction = Atan(argument.simplify)
  
    override def toString: String = s"atan($argument)"
  }
  
  /** A function which is the hyperbolic sine of another.
    *
    * When evaluated, the result will be the hyperbolic sine of the result of evaluating the other function.
    * @param argument
    *          The function of which this is the hyperbolic sine
    */
  case class Sinh(argument: ComplexFunction) extends UnaryFunction(_.sinh) {
    override def derivative(implicit respect: String): ComplexFunction = Cosh(argument) * argument.derivative
    
    override def simplify: ComplexFunction = argument.simplify match {
      case Asinh(x) => x
      case x        => Sinh(x)
    }
  
    override def toString: String = s"sinh($argument)"
  }
  
  /** A function which is the inverse hyperbolic sine of another.
    *
    * When evaluated, the result will be the inverse hyperbolic sine of the result of evaluating the other function.
    * @param argument
    *          The function of which this is the inverse hyperbolic sine
    */
  case class Asinh(argument: ComplexFunction) extends UnaryFunction(_.asinh) {
    override def derivative(implicit respect: String): ComplexFunction = argument.derivative / Sqrt((argument ~^ 2) + 1)
    
    override def simplify: ComplexFunction = Asinh(argument.simplify)
  
    override def toString: String = s"asinh($argument)"
  }
  
  /** A function which is the hyperbolic cosine of another.
    *
    * When evaluated, the result will be the hyperbolic cosine of the result of evaluating the other function.
    * @param argument
    *          The function of which this is the hyperbolic cosine
    */
  case class Cosh(argument: ComplexFunction) extends UnaryFunction(_.cosh) {
    override def derivative(implicit respect: String): ComplexFunction = Sinh(argument) * argument.derivative
    
    override def simplify: ComplexFunction = argument.simplify match {
      case Acosh(x) => x
      case x        => Cosh(x)
    }
  
    override def toString: String = s"cosh($argument)"
  }
  
  /** A function which is the inverse hyperbolic cosine of another.
    *
    * When evaluated, the result will be the inverse hyperbolic cosine of the result of evaluating the other function.
    * @param argument
    *          The function of which this is the inverse hyperbolic cosine
    */
  case class Acosh(argument: ComplexFunction) extends UnaryFunction(_.acosh) {
    override def derivative(implicit respect: String): ComplexFunction = argument.derivative / Sqrt((argument ~^ 2) - 1)
    
    override def simplify: ComplexFunction = Acosh(argument.simplify)
  
    override def toString: String = s"acosh($argument)"
  }
  
  /** A function which is the hyperbolic tangent of another.
    *
    * When evaluated, the result will be the hyperbolic tangent of the result of evaluating the other function.
    * @param argument
    *          The function of which this is the hyperbolic tangent
    */
  case class Tanh(argument: ComplexFunction) extends UnaryFunction(_.tanh) {
    override def derivative(implicit respect: String): ComplexFunction = argument.derivative / (Sinh(argument) ~^ 2)
    
    override def simplify: ComplexFunction = argument.simplify match {
      case Atanh(x) => x
      case x        => Tanh(x)
    }
  
    override def toString: String = s"tanh($argument)"
  }
  
  /** A function which is the inverse hyperbolic tangent of another.
    *
    * When evaluated, the result will be the inverse hyperbolic tangent of the result of evaluating the other function.
    * @param argument
    *          The function of which this is the inverse hyperbolic tangent
    */
  case class Atanh(argument: ComplexFunction) extends UnaryFunction(_.atanh) {
    override def derivative(implicit respect: String): ComplexFunction = argument.derivative / (1 - (argument ~^ 2))
    
    override def simplify: ComplexFunction = Atanh(argument.simplify)
  
    override def toString: String = s"atanh($argument)"
  }
}