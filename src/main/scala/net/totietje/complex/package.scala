package net.totietje

/** This package is centered around the [[net.totietje.complex.Complex Complex]] class, with
  * a focus on complex arithmetic.
  *
  * The [[net.totietje.complex.ComplexEvaluator ComplexEvaluator]] object contains the `evaluate(String)` method
  * for parsing a mathematical expression as a function of complex numbers.
  *
  * @example val sine = Complex(3, 4).sin
  * @example val function = ComplexEvaluator.parse("log(i * x) - i * log(y)")
  */
package object complex