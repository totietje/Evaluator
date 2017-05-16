package net.totietje.complex

import net.totietje.evaluator.Associativity
import net.totietje.evaluator.Token._

import collection.immutable.IndexedSeq

private object ComplexFunctionToken {
  case class Constant(complex: Complex) extends Value[ComplexFunction] {
    override def apply(): ComplexFunction = complex
  }
  case class Variable(variable: String) extends Value[ComplexFunction] {
    override def apply(): ComplexFunction = ComplexFunction.Variable(variable)
  }
  
  case object Plus extends Operator[ComplexFunction](0, Associativity.Left) {
    override def apply(left: ComplexFunction, right: ComplexFunction): ComplexFunction = left + right
  }
  case object Minus extends Operator[ComplexFunction](0, Associativity.Left) {
    override def apply(left: ComplexFunction, right: ComplexFunction): ComplexFunction = left - right
  }
  case object Multiply extends Operator[ComplexFunction](1, Associativity.Left) {
    override def apply(left: ComplexFunction, right: ComplexFunction): ComplexFunction = left * right
  }
  case object Divide extends Operator[ComplexFunction](1, Associativity.Left) {
    override def apply(left: ComplexFunction, right: ComplexFunction): ComplexFunction = left / right
  }
  case object Power extends Operator[ComplexFunction](2, Associativity.Left) {
    override def apply(left: ComplexFunction, right: ComplexFunction): ComplexFunction = left ~^ right
  }
  
  case object Log extends Function[ComplexFunction](1) {
    override def apply(args: IndexedSeq[ComplexFunction]): ComplexFunction = ComplexFunction.Log(args(0))
  }
  case object Sqrt extends Function[ComplexFunction](1) {
    override def apply(args: IndexedSeq[ComplexFunction]): ComplexFunction = ComplexFunction.Sqrt(args(0))
  }
  case object Arg extends Function[ComplexFunction](1) {
    override def apply(args: IndexedSeq[ComplexFunction]): ComplexFunction = ComplexFunction.Arg(args(0))
  }
  case object Abs extends Function[ComplexFunction](1) {
    override def apply(args: IndexedSeq[ComplexFunction]): ComplexFunction = ComplexFunction.Abs(args(0))
  }
  case object Conj extends Function[ComplexFunction](1) {
    override def apply(args: IndexedSeq[ComplexFunction]): ComplexFunction = ComplexFunction.Conj(args(0))
  }
  case object Re extends Function[ComplexFunction](1) {
    override def apply(args: IndexedSeq[ComplexFunction]): ComplexFunction = ComplexFunction.Re(args(0))
  }
  case object Im extends Function[ComplexFunction](1) {
    override def apply(args: IndexedSeq[ComplexFunction]): ComplexFunction = ComplexFunction.Im(args(0))
  }
  case object UnaryMinus extends Function[ComplexFunction](1) {
    override def apply(args: IndexedSeq[ComplexFunction]): ComplexFunction = -args(0)
  }
  case object UnaryPlus extends Function[ComplexFunction](1) {
    override def apply(args: IndexedSeq[ComplexFunction]): ComplexFunction = args(0)
  }
  
  case object Sin extends Function[ComplexFunction](1) {
    override def apply(args: IndexedSeq[ComplexFunction]): ComplexFunction = ComplexFunction.Sin(args(0))
  }
  case object Cos extends Function[ComplexFunction](1) {
    override def apply(args: IndexedSeq[ComplexFunction]): ComplexFunction = ComplexFunction.Cos(args(0))
  }
  case object Tan extends Function[ComplexFunction](1) {
    override def apply(args: IndexedSeq[ComplexFunction]): ComplexFunction = ComplexFunction.Tan(args(0))
  }
  case object Asin extends Function[ComplexFunction](1) {
    override def apply(args: IndexedSeq[ComplexFunction]): ComplexFunction = ComplexFunction.Asin(args(0))
  }
  case object Acos extends Function[ComplexFunction](1) {
    override def apply(args: IndexedSeq[ComplexFunction]): ComplexFunction = ComplexFunction.Acos(args(0))
  }
  case object Atan extends Function[ComplexFunction](1) {
    override def apply(args: IndexedSeq[ComplexFunction]): ComplexFunction = ComplexFunction.Atan(args(0))
  }
  case object Sinh extends Function[ComplexFunction](1) {
    override def apply(args: IndexedSeq[ComplexFunction]): ComplexFunction = ComplexFunction.Sinh(args(0))
  }
  case object Cosh extends Function[ComplexFunction](1) {
    override def apply(args: IndexedSeq[ComplexFunction]): ComplexFunction = ComplexFunction.Cosh(args(0))
  }
  case object Tanh extends Function[ComplexFunction](1) {
    override def apply(args: IndexedSeq[ComplexFunction]): ComplexFunction = ComplexFunction.Tanh(args(0))
  }
  case object Asinh extends Function[ComplexFunction](1) {
    override def apply(args: IndexedSeq[ComplexFunction]): ComplexFunction = ComplexFunction.Asinh(args(0))
  }
  case object Acosh extends Function[ComplexFunction](1) {
    override def apply(args: IndexedSeq[ComplexFunction]): ComplexFunction = ComplexFunction.Acosh(args(0))
  }
  case object Atanh extends Function[ComplexFunction](1) {
    override def apply(args: IndexedSeq[ComplexFunction]): ComplexFunction = ComplexFunction.Atanh(args(0))
  }
}
