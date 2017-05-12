package net.totietje.evaluator

import net.totietje.evaluator.ComplexFunction._
import net.totietje.evaluator.Token._

object ComplexFunctionToken {
  case class Constant(complex: Complex) extends Value[ComplexFunction] {
    override def apply(): ComplexFunction = complex
  }
  case class Variable(variable: String) extends Value[ComplexFunction] {
    override def apply(): ComplexFunction = ComplexFunction.Variable(variable)
  }
  
  case object PLUS extends Operator[ComplexFunction](0, Associativity.LEFT) {
    override def apply(left: ComplexFunction, right: ComplexFunction): ComplexFunction = left + right
  }
  case object MINUS extends Operator[ComplexFunction](0, Associativity.LEFT) {
    override def apply(left: ComplexFunction, right: ComplexFunction): ComplexFunction = left - right
  }
  case object MULTIPLY extends Operator[ComplexFunction](1, Associativity.LEFT) {
    override def apply(left: ComplexFunction, right: ComplexFunction): ComplexFunction = left * right
  }
  case object DIVIDE extends Operator[ComplexFunction](1, Associativity.LEFT) {
    override def apply(left: ComplexFunction, right: ComplexFunction): ComplexFunction = left / right
  }
  case object POWER extends Operator[ComplexFunction](2, Associativity.LEFT) {
    override def apply(left: ComplexFunction, right: ComplexFunction): ComplexFunction = left ~^ right
  }
  
  case object LOG extends Function[ComplexFunction](1) {
    override def apply(args: ComplexFunction*): ComplexFunction = Log(args(0))
  }
  case object SQRT extends Function[ComplexFunction](1) {
    override def apply(args: ComplexFunction*): ComplexFunction = Sqrt(args(0))
  }
  case object ARG extends Function[ComplexFunction](1) {
    override def apply(args: ComplexFunction*): ComplexFunction = Arg(args(0))
  }
  case object ABS extends Function[ComplexFunction](1) {
    override def apply(args: ComplexFunction*): ComplexFunction = Abs(args(0))
  }
  case object CONJ extends Function[ComplexFunction](1) {
    override def apply(args: ComplexFunction*): ComplexFunction = Conj(args(0))
  }
  case object RE extends Function[ComplexFunction](1) {
    override def apply(args: ComplexFunction*): ComplexFunction = Re(args(0))
  }
  case object IM extends Function[ComplexFunction](1) {
    override def apply(args: ComplexFunction*): ComplexFunction = Im(args(0))
  }
  case object UNARY_MINUS extends Function[ComplexFunction](1) {
    override def apply(args: ComplexFunction*): ComplexFunction = -args(0)
  }
  case object UNARY_PLUS extends Function[ComplexFunction](1) {
    override def apply(args: ComplexFunction*): ComplexFunction = args(0)
  }
  
  case object SIN extends Function[ComplexFunction](1) {
    override def apply(args: ComplexFunction*): ComplexFunction = Sin(args(0))
  }
  case object COS extends Function[ComplexFunction](1) {
    override def apply(args: ComplexFunction*): ComplexFunction = Cos(args(0))
  }
  case object TAN extends Function[ComplexFunction](1) {
    override def apply(args: ComplexFunction*): ComplexFunction = Tan(args(0))
  }
  case object ASIN extends Function[ComplexFunction](1) {
    override def apply(args: ComplexFunction*): ComplexFunction = Asin(args(0))
  }
  case object ACOS extends Function[ComplexFunction](1) {
    override def apply(args: ComplexFunction*): ComplexFunction = Acos(args(0))
  }
  case object ATAN extends Function[ComplexFunction](1) {
    override def apply(args: ComplexFunction*): ComplexFunction = Atan(args(0))
  }
  case object SINH extends Function[ComplexFunction](1) {
    override def apply(args: ComplexFunction*): ComplexFunction = Sinh(args(0))
  }
  case object COSH extends Function[ComplexFunction](1) {
    override def apply(args: ComplexFunction*): ComplexFunction = Cosh(args(0))
  }
  case object TANH extends Function[ComplexFunction](1) {
    override def apply(args: ComplexFunction*): ComplexFunction = Tanh(args(0))
  }
  case object ASINH extends Function[ComplexFunction](1) {
    override def apply(args: ComplexFunction*): ComplexFunction = Asinh(args(0))
  }
  case object ACOSH extends Function[ComplexFunction](1) {
    override def apply(args: ComplexFunction*): ComplexFunction = Acosh(args(0))
  }
  case object ATANH extends Function[ComplexFunction](1) {
    override def apply(args: ComplexFunction*): ComplexFunction = Atanh(args(0))
  }
}
