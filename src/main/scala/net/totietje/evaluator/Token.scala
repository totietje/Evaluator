package net.totietje.evaluator

import collection.immutable.IndexedSeq

/** Represents one of the base parts of an expression parsed by an [[net.totietje.evaluator.Evaluator Evaluator]].
  * @tparam R
  *           The type that the evaluator should return after evaluating the tokens
  */
sealed trait Token[R]

/** Contains all the different types of tokens.
  */
object Token {
  private[evaluator] sealed trait Postfix[R] extends Token[R]
  
  private[evaluator] sealed trait Precedence[R] extends Token[R] {
    def precedence : Int
  }
  
  /** Represents an operator.
    *
    * An operator is something that takes in the values on either side of it as arguments. For example, in the
    * expression `3 + 4`, `+` is an operator that takes in the arguments `3` and `4`.
    *
    * Operators also have `precedence`. Operators with higher precedence are evaluated first. For example,
    * the expression `3 + 4 * 2`, is interpreted as `3 + (4 * 2)` because the `*` operator has higher precedence than
    * the `+` operator.
    * @param precedence
    *                   The precedence of this operator. This cannot be `Int.MinValue` or `Int.MaxValue`,
    *                   as these are reserved for brackets and functions respectively.
    * @param associativity
    *                      The associativity of the token - either `Associativity.Left` or `Associativity.Right`.
    *                      See [[net.totietje.evaluator.Associativity Associativity]] for more information.
    * @tparam R
    *           The type that the evaluator should return after evaluating the tokens
    */
  abstract class Operator[R](val precedence: Int, val associativity: Associativity)
      extends Postfix[R] with Precedence[R] {
    /** Returns the result of applying this operator on two values.
      * @param left
      *             The value to the left of the operator
      * @param right
      *              The value to the right of the operator
      * @return
      *         The result of applying the operator
      */
    def apply(left: R, right: R) : R
  }
  
  /** Represents a function.
    *
    * A function can have zero or more arguments. Immediately following the function, there should be a pair of
    * brackets, within which there should be the arguments to the function. The arguments to the function should be
    * separated by the [[net.totietje.evaluator.Token.ArgSeparator ArgSeparator]] token.
    *
    * For example, in `sin(4)`, `sin` is a function that accepts one argument. Similarly, in `max(3, 4)`, `max` is
    * a function that accepts two arguments, and '`,`' is an argument separator.
    * @param args
    *             The number of arguments this function accepts
    * @tparam R
    *           The type that the evaluator should return after evaluating the tokens
    */
  abstract class Function[R](val args: Int) extends Precedence[R] with Postfix[R] {
    /** The precedence of the function, that is, the priority with which it should be evaluated.
      *
      * By default this is `Int.MaxValue` to indicate highest priority.
      * @return
      *         The precedence of the function
      */
    override def precedence: Int = Int.MaxValue
  
    /** Applies the function on the given arguments and returns the result.
      * @param args
      *             The arguments to apply the function on. The length of this parameter will always be the number
      *             of arguments this function accepts.
      * @return
      *         The result of applying the function on the input arguments
      */
    def apply(args: IndexedSeq[R]): R
  }
  
  /** Represents a value.
    *
    * A value is anything that stands on its own, and can be evaluated without any arguments.
    *
    * For example, in the expression `2 + x`, both `2` and `x` are values - the former a constant and the latter
    * a variable.
    *
    * The value of the function doesn't need to be fixed. For example, there may be a value `rand` which returns a
    * random number between 0 and 1 every time it's evaluated.
    * @tparam R
    *           The type that the evaluator should return after evaluating the tokens
    */
  abstract class Value[R] extends Postfix[R] {
    /** Returns any value.
      *
      * Multiple invocations of this method need not return the same value.
      * @return
      *         A value
      */
    def apply(): R
  }
  
  /** Either an open or close parenthesis.
    *
    * Parentheses are used to separate sections of an expression and indicate it acts as one value. For example,
    * in `4 * (2 + 3)`, the parentheses indicate that the `2 + 3` should be evaluated first.
    *
    * Parentheses are also used after [[net.totietje.evaluator.Token.Function functions]].
    * @tparam R
    *           The type that the evaluator should return after evaluating the tokens
    */
  sealed abstract class Parenthesis[R] extends Token[R] with Precedence[R] {
    /** The precedence of the parenthesis.
      *
      * This will return `Int.MinValue` to indicate no priority.
      * @return
      *         `Int.MinValue`
      */
    override final def precedence: Int = Int.MinValue
  }
  
  /** An argument separator.
    *
    * This distinguishes between multiple arguments in a [[net.totietje.evaluator.Token.Function Function]].
    * @tparam R
    *           The type that the evaluator should return after evaluating the tokens
    */
  case class ArgSeparator[R]() extends Token[R]
  
  /** An open parenthesis.
    * @tparam R
    *           The type that the evaluator should return after evaluating the tokens
    */
  case class OpenParen[R]() extends Parenthesis[R]
  
  /** A close parenthesis.
    * @tparam R
    *           The type that the evaluator should return after evaluating the tokens
    */
  case class CloseParen[R]() extends Parenthesis[R]
}