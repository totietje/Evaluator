package net.totietje.evaluator

sealed trait Token

object Token {
  private[evaluator] sealed trait Postfix extends Token
  
  sealed trait Precedence extends Token {
    def precedence : Int
  }
  
  sealed abstract class Parenthesis extends Precedence
  
  abstract class Operator[R](val precedence: Int, val associativity: Associativity) extends Precedence with Postfix {
    def apply(left: R, right: R) : R
  }
  
  abstract class Function[R](val args: Int) extends Precedence with Postfix {
    override def precedence: Int = Int.MaxValue
    
    def apply(args: R*): R
  }
  
  abstract class Value[R] extends Postfix {
    def apply(): R
  }
  
  object ARG_SEPARATOR extends Token
  
  object OPEN_PAREN extends Parenthesis {
    override def precedence: Int = Int.MinValue
  }
  object CLOSE_PAREN extends Parenthesis {
    override def precedence: Int = Int.MinValue
  }
}

sealed trait Associativity

object Associativity {
  final case object LEFT extends Associativity
  final case object RIGHT extends Associativity
}