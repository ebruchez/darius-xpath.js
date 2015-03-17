package client.net.sf.saxon.ce.pattern

import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.functions.Id
import client.net.sf.saxon.ce.functions.KeyFn
import client.net.sf.saxon.ce.js.IXSLFunction
import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Parser for XSLT patterns. This is created by overriding selected parts of the standard ExpressionParser.
 */
class PatternParser extends ExpressionParser {

  var inPredicate: Int = 0

  /**
   * Parse a string representing an XSLT pattern
   * @param pattern the pattern expressed as a String
   * @param env     the static context for the pattern
   * @return a Pattern object representing the result of parsing
   * @throws XPathException if the pattern contains a syntax error
   */
  def parsePattern(pattern: String, env: StaticContext): Pattern = {
    this.env = env
    language = XSLT_PATTERN
    val exp = parse(pattern, 0, Token.EOF, env)
    exp.setContainer(defaultContainer)
    val visitor = ExpressionVisitor.make(env, exp.getExecutable)
    Pattern.fromExpression(exp.simplify(visitor), env.getConfiguration)
  }

  def parseExpression(): Expression = {
    if (inPredicate > 0) {
      super.parseExpression()
    } else {
      parseBinaryExpression(parsePathExpression(), 10)
    }
  }

  /**
   * Parse a basic step expression (without the predicates)
   * @param firstInPattern true only if we are parsing the first step in a
   *                       RelativePathPattern in the XSLT Pattern syntax
   * @return the resulting subexpression
   * @throws XPathException if any error is encountered
   */
  protected def parseBasicStep(firstInPattern: Boolean): Expression = {
    if (inPredicate > 0) {
      super.parseBasicStep(firstInPattern)
    } else t.currentToken match {
      case Token.LPAR | Token.STRING_LITERAL | Token.NUMBER => 
        grumble("Token " + currentTokenDisplay() + " not allowed here in an XSLT pattern")
        null

      case Token.FUNCTION => 
        if (!firstInPattern) {
          grumble("In an XSLT pattern, a function call is allowed only as the first step in a path")
        }
        super.parseBasicStep(firstInPattern)

      case _ => super.parseBasicStep(firstInPattern)
    }
  }

  protected def parsePredicate(): Expression = {
    inPredicate
    val exp = parseExpression()
    inPredicate
    exp
  }

  protected def parseFunctionCall(): Expression = {
    val fn = super.parseFunctionCall()
    if (inPredicate > 0) {
      return fn
    } else {
      if (fn.isInstanceOf[Id]) {
        if (fn.asInstanceOf[Id].getNumberOfArguments != 1) {
          grumble("id() in an XSLT 2.0 pattern must have only one argument")
        } else {
          val arg = fn.asInstanceOf[Id].getArguments()(0)
          if (!(arg.isInstanceOf[VariableReference] || arg.isInstanceOf[StringLiteral])) {
            grumble("Argument to id() in a pattern must be a variable reference or string literal")
          }
        }
      } else if (fn.isInstanceOf[KeyFn]) {
        if (fn.asInstanceOf[KeyFn].getNumberOfArguments != 2) {
          grumble("key() in an XSLT 2.0 pattern must have exactly two arguments")
        } else {
          val arg0 = fn.asInstanceOf[KeyFn].getArguments()(0)
          if (!(arg0.isInstanceOf[StringLiteral])) {
            grumble("First argument to key() in an XSLT 2.0 pattern must be a string literal")
          }
          val arg1 = fn.asInstanceOf[KeyFn].getArguments()(1)
          if (!(arg1.isInstanceOf[VariableReference] || arg1.isInstanceOf[Literal])) {
            grumble("Second argument to id() in an XSLT 2.0 pattern must be a variable reference or literal")
          }
        }
      } else if (fn.isInstanceOf[IXSLFunction]) {
        return fn
      } else {
        grumble("The " + fn.toString + 
          " function is not allowed at the head of a pattern")
      }
    }
    fn
  }

  protected def parseFunctionArgument(): Expression = {
    if (inPredicate > 0) {
      super.parseFunctionArgument()
    } else t.currentToken match {
      case Token.DOLLAR => parseVariableReference()
      case Token.STRING_LITERAL => parseStringLiteral()
      case Token.NUMBER => parseNumericLiteral()
      case _ => 
        grumble("A function argument in an XSLT pattern must be a variable reference or literal")
        null

    }
  }
}
