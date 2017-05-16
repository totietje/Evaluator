import net.totietje.evaluator.Token._
import net.totietje.evaluator.Associativity

import collection.immutable.IndexedSeq

object Test {
  def main(args: Array[String]): Unit = {
    println(BooleanEvaluator.evaluate("(true | false) & !false"))
  }
}

object BooleanToken {
  //0 is the precedence. This does not matter much here, but may in more complicated examples.
  object And extends Operator[Boolean](0, Associativity.Left) {
    override def apply(left: Boolean, right: Boolean): Boolean = left & right
  }
  
  object Or extends Operator[Boolean](0, Associativity.Left) {
    override def apply(left: Boolean, right: Boolean): Boolean = left | right
  }
  
  //This function accepts 1 parameter
  object Not extends Function[Boolean](1) {
    //args will have length 1
    override def apply(args: IndexedSeq[Boolean]): Boolean = !args(0)
  }
  
  object True extends Value[Boolean] {
    override def apply(): Boolean = true
  }
  
  object False extends Value[Boolean] {
    override def apply(): Boolean = false
  }
}

import BooleanToken._
import net.totietje.evaluator.Token._
import net.totietje.evaluator.{EvaluationException, AbstractEvaluator, Token}

object BooleanEvaluator extends AbstractEvaluator[Boolean] {
  override protected def parseOtherChar(char: Char): Option[Token[Boolean]] = char match {
    case '!' => Some(Not)
    case '(' => Some(OpenParen())
    case _   => None
  }
  
  override protected def parseAfterValueChar(op: Char): Option[Token[Boolean]] = op match {
    case '&' => Some(And)
    case '|' => Some(Or)
    case ')' => Some(CloseParen())
    case _   => None
  }
  
  override protected def parseWord(word: String): Token[Boolean] = word.toLowerCase match {
    case "true" => True
    case "false" => False
    case _ => throw EvaluationException(s"Unrecognised word $word")
  }
}