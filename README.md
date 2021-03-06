# Evaluator
A Scala expression parser.

## Guide

### Creating an Evaluator

The `AbstractEvaluator` class is designed to simplify the process of parsing a string. The subclass must simply
override the `tokenizer` method. This can be further simplified with the `AbstractTokenizer` class, if applicable.

For example, let's make a `BooleanEvaluator` object. It's purpose will be to evaluate a string as a `Boolean`, and
should contain the operators & (and), | (or), ^ (xor), and ! (not). It must also be able to handle parentheses. So,
`!true & !false` should return the boolean value `false`.

```scala
//First, we define the tokens we use:
import net.totietje.evaluator.Token._
import net.totietje.evaluator.Associativity

object BooleanToken {
  //0 is the precedence. This does not matter much here, but may in more complicated examples.
  object And extends Operator[Boolean](0, Associativity.Left) {
    override def apply(left: Boolean, right: Boolean): Boolean = left & right
  }
  
  object Or extends Operator[Boolean](1, Associativity.Left) {
    override def apply(left: Boolean, right: Boolean): Boolean = left | right
  }
  
  object Xor extends Operator[Boolean](2, Associativity.Left) {
    override def apply(left: Boolean, right: Boolean): Boolean = left ^ right
  }
  
  //This function accepts 1 parameter
  object Not extends Function[Boolean](1) {
    //args will have length 1 
    override def apply(args: Seq[Boolean]): Boolean = !args.head
  }
  
  object True extends Value[Boolean] {
    override def apply(): Boolean = true
  }
  
  object False extends Value[Boolean] {
    override def apply(): Boolean = false
  }
}

//Then, in another file, we can use these tokens:
import BooleanToken._
import net.totietje.evaluator.Token._
import net.totietje.evaluator.{EvaluationException, AbstractTokenizer, Token}

object BooleanTokenizer extends AbstractTokenizer[Boolean] {
  override protected def parseAfterValueChar(op: Char): Option[Token[Boolean]] = op match {
    case '&' => Some(And)
    case '|' => Some(Or)
    case ')' => Some(CloseParen()) //CloseParen case class provided by Token
    case _   => None
  }

  override protected def parseOtherChar(char: Char): Option[Token[Boolean]] = char match {
    case '!' => Some(Not)  
    case '(' => Some(OpenParen()) //OpenParen case class provided by Token
    case _   => None
  }
  
  override protected def parseWord(word: String): Token[Boolean] = word.toLowerCase match {
    case "true" => True
    case "false" => False
    case _ => throw EvaluationException(s"Unrecognised word $word")
  }
}

//Finally, we can create our BooleanEvaluator:
import net.totietje.evaluator.AbstractEvaluator

object BooleanEvaluator extends AbstractEvaluator[Boolean] {
  def tokenizer = BooleanTokenizer
}
```

Now, we can use our `BooleanEvaluator`:

```scala
val example1 = BooleanEvaluator.evaluate("(true | false) & !false") //true, as expected
```

### ComplexEvaluator

As an example (though mainly because this is why I wanted to make this project), the `net.totietje.complex` package is
included containing the `ComplexEvaluator` object. This parses a maths expression, such as `a + b * i`, and
converts it into a `ComplexFunction`, which takes a map of variables as its input and produces a complex number
as its output.

Example usage:

```scala
import net.totietje.complex._

val function = ComplexEvaluator.evaluate("i * sin(pi * x)")
for (realPart <- -5 to 5; imaginaryPart <- -5 to 5) {
  val input = Complex(realPart, imaginaryPart)
  val output = function("x" -> input)
  println(s"When x is $input, function is $output")
}
```

## Download

[ ![Download](https://api.bintray.com/packages/totietje/maven/evaluator/images/download.svg) ](https://bintray.com/totietje/maven/evaluator/_latestVersion)

Scala 2.10, 2.11 or 2.12 required. If the scala version used is not 2.12, replace `2.12` with the version you are using
below.

Replace `VERSION` with the version shown above.

SBT:

```
dependencies += "net.totietje" %% "evaluator" % "VERSION"

resolvers += Resolver.jcenterRepo
```

Gradle:
```
dependencies {
    compile 'net.totietje:evaluator_2.12:VERSION'
}

repositories {
    jcenter()
}
```

Maven:

```xml
<dependency>
  <groupId>net.totietje</groupId>
  <artifactId>evaluator_2.12</artifactId>
  <version>VERSION</version>
  <type>pom</type>
</dependency>

<repository>
    <id>jcenter</id>
    <name>jcenter-bintray</name>
    <url>http://jcenter.bintray.com</url>
</repository>
```

Ivy:

```xml
<dependency org='net.totietje' name='evaluator_2.12' rev='VERSION'>
  <artifact name='evaluator_2.12' ext='pom' />
</dependency>

<resolvers>
  <bintray />
</resolvers>
```

## Docs

Docs can be found [here](https://totietje.github.io/Evaluator/).