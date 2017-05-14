package net.totietje.evaluator

import collection.immutable.IndexedSeq

sealed trait Token[+R]

object Token {
  private[evaluator] sealed trait Postfix[R] extends Token[R]
  
  sealed trait Precedence[R] extends Token[R] {
    def precedence : Int
  }
  
  sealed abstract class Parenthesis extends Precedence[Any]
  
  abstract class Operator[R](val precedence: Int, val associativity: Associativity) extends Precedence[R] with Postfix[R] {
    def apply(left: R, right: R) : R
  }
  
  abstract class Function[R](val args: Int) extends Precedence[R] with Postfix[R] {
    override def precedence: Int = Int.MaxValue
    
    def apply(args: IndexedSeq[R]): R
  }
  
  abstract class Value[R] extends Postfix[R] {
    def apply(): R
  }
  
  object ARG_SEPARATOR extends Token[Any]
  
  object OPEN_PAREN extends Parenthesis {
    override def precedence: Int = Int.MinValue
  }
  object CLOSE_PAREN extends Parenthesis {
    override def precedence: Int = Int.MinValue
  }
}

sealed trait Associativity

object Associativity {
  case object LEFT extends Associativity
  case object RIGHT extends Associativity
}