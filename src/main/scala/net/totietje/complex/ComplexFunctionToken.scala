package net.totietje.complex

import net.totietje.evaluator.Associativity
import net.totietje.evaluator.Token._

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
    override def apply(args: Seq[ComplexFunction]): ComplexFunction = ComplexFunction.Log(args.head)
  }
  case object Sqrt extends Function[ComplexFunction](1) {
    override def apply(args: Seq[ComplexFunction]): ComplexFunction = ComplexFunction.Sqrt(args.head)
  }
  case object Arg extends Function[ComplexFunction](1) {
    override def apply(args: Seq[ComplexFunction]): ComplexFunction = ComplexFunction.Arg(args.head)
  }
  case object Abs extends Function[ComplexFunction](1) {
    override def apply(args: Seq[ComplexFunction]): ComplexFunction = ComplexFunction.Abs(args.head)
  }
  case object Conj extends Function[ComplexFunction](1) {
    override def apply(args: Seq[ComplexFunction]): ComplexFunction = ComplexFunction.Conj(args.head)
  }
  case object Re extends Function[ComplexFunction](1) {
    override def apply(args: Seq[ComplexFunction]): ComplexFunction = ComplexFunction.Re(args.head)
  }
  case object Im extends Function[ComplexFunction](1) {
    override def apply(args: Seq[ComplexFunction]): ComplexFunction = ComplexFunction.Im(args.head)
  }
  case object UnaryMinus extends Function[ComplexFunction](1) {
    override def apply(args: Seq[ComplexFunction]): ComplexFunction = -args.head
  }
  case object UnaryPlus extends Function[ComplexFunction](1) {
    override def apply(args: Seq[ComplexFunction]): ComplexFunction = args.head
  }
  
  case object Sin extends Function[ComplexFunction](1) {
    override def apply(args: Seq[ComplexFunction]): ComplexFunction = ComplexFunction.Sin(args.head)
  }
  case object Cos extends Function[ComplexFunction](1) {
    override def apply(args: Seq[ComplexFunction]): ComplexFunction = ComplexFunction.Cos(args.head)
  }
  case object Tan extends Function[ComplexFunction](1) {
    override def apply(args: Seq[ComplexFunction]): ComplexFunction = ComplexFunction.Tan(args.head)
  }
  case object Asin extends Function[ComplexFunction](1) {
    override def apply(args: Seq[ComplexFunction]): ComplexFunction = ComplexFunction.Asin(args.head)
  }
  case object Acos extends Function[ComplexFunction](1) {
    override def apply(args: Seq[ComplexFunction]): ComplexFunction = ComplexFunction.Acos(args.head)
  }
  case object Atan extends Function[ComplexFunction](1) {
    override def apply(args: Seq[ComplexFunction]): ComplexFunction = ComplexFunction.Atan(args.head)
  }
  case object Sinh extends Function[ComplexFunction](1) {
    override def apply(args: Seq[ComplexFunction]): ComplexFunction = ComplexFunction.Sinh(args.head)
  }
  case object Cosh extends Function[ComplexFunction](1) {
    override def apply(args: Seq[ComplexFunction]): ComplexFunction = ComplexFunction.Cosh(args.head)
  }
  case object Tanh extends Function[ComplexFunction](1) {
    override def apply(args: Seq[ComplexFunction]): ComplexFunction = ComplexFunction.Tanh(args.head)
  }
  case object Asinh extends Function[ComplexFunction](1) {
    override def apply(args: Seq[ComplexFunction]): ComplexFunction = ComplexFunction.Asinh(args.head)
  }
  case object Acosh extends Function[ComplexFunction](1) {
    override def apply(args: Seq[ComplexFunction]): ComplexFunction = ComplexFunction.Acosh(args.head)
  }
  case object Atanh extends Function[ComplexFunction](1) {
    override def apply(args: Seq[ComplexFunction]): ComplexFunction = ComplexFunction.Atanh(args.head)
  }
}
