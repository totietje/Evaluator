package net.totietje.evaluator

/** This represents the associativity of an operator.
  *
  * Operators can be either left associative or right associative. This indicated the order in which operators
  * of the same precedence are evaluated in the absence of parentheses - from left to right or from right to left.
  *
  * For example, the `/` operator is left associative, so {{{2 / 2 / 2 = (2 / 2) / 2}}}
  *
  * However, the `&#94;` operator is right associative, so {{{2 ^ 2 ^ 2 = 2 ^ (2 ^ 2)}}}
  */
sealed trait Associativity

/** Contains the instances of the associativity trait.
  */
object Associativity {
  
  /** Represents left associativity.
    *
    * If an operator is left associative, then the operators are grouped from left to right.
    */
  case object Left extends Associativity
  
  /** Represents right associativity.
    *
    * If an operator is right associative, then the operators are grouped from right to left.
    */
  case object Right extends Associativity
}