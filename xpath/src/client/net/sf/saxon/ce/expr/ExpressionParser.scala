// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.`type`._
import client.net.sf.saxon.ce.expr.ExpressionParser._
import client.net.sf.saxon.ce.expr.instruct.{Block, Choose}
import client.net.sf.saxon.ce.functions.SystemFunction
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om._
import client.net.sf.saxon.ce.orbeon.{ArrayList, Stack}
import client.net.sf.saxon.ce.pattern._
import client.net.sf.saxon.ce.trans.{Err, XPathException}
import client.net.sf.saxon.ce.value._

import scala.util.control.Breaks._

object ExpressionParser {

  val XPATH = 0
  val XSLT_PATTERN = 1
  val SEQUENCE_TYPE = 2

  class ForClause {

    var rangeVariable: Assignation = _

    var sequence: Expression = _

    var requiredType: SequenceType = _

    var offset: Int = _

    def numberOfRangeVariables(): Int = 1
  }
}

/**
 * Parser for XPath expressions and XSLT patterns.
 *
 * This code was originally inspired by James Clark's xt but has been totally rewritten (several times)
 *
 * The class handles parsing of XPath 2.0 syntax.
 */
class ExpressionParser {

  protected var t: Tokenizer = _

  protected var env: StaticContext = _

  protected var rangeVariables = new Stack[Binding]()

  protected var defaultContainer: Container = _

  protected var language: Int = XPATH

  /**
   * Get the tokenizer (the lexical analyzer)
   * @return  the tokenizer (the lexical analyzer)
   */
  def getTokenizer(): Tokenizer = t

  /**
   * Get the static context used by this expression parser
   * @return the static context
   */
  def getStaticContext(): StaticContext = env

  /**
   * Set the default container for newly constructed expressions
   * @param container the default container
   */
  def setDefaultContainer(container: Container): Unit = {
    this.defaultContainer = container
  }

  /**
   * Read the next token, catching any exception thrown by the tokenizer
   */
  def nextToken(): Unit = {
    try {
      t.next()
    } catch {
      case err: XPathException => grumble(err.getMessage)
    }
  }

  /**
   * Expect a given token; fail if the current token is different. Note that this method
   * does not read any tokens.
   *
   * @param token the expected token
   * @throws XPathException if the current token is not the expected
   *     token
   */
  def expect(token: Int): Unit = {
    if (t.currentToken != token) grumble("expected \"" + Token.tokens(token) + "\", found " + currentTokenDisplay())
  }

  /**
   * Report a syntax error (a static error with error code XP0003)
   * @param message the error message
   * @throws XPathException always thrown: an exception containing the
   *     supplied message
   */
  def grumble(message: String): Nothing = {
    grumble(message, if (language == XSLT_PATTERN) "XTSE0340" else "XPST0003")
  }

  /**
   * Report a static error
   *
   * @param message the error message
   * @param errorCode the error code
   * @throws XPathException always thrown: an exception containing the
   *                                        supplied message
   */
  def grumble(message: String, errorCode: String): Nothing = {
    grumble(message, new StructuredQName("", NamespaceConstant.ERR, errorCode))
  }

  /**
   * Report a static error
   *
   * @param message the error message
   * @param _errorCode the error code
   * @throws XPathException always thrown: an exception containing the
   *     supplied message
   */
  protected def grumble(message: String, _errorCode: StructuredQName): Nothing = {
    var errorCode = _errorCode
    if (errorCode == null) {
      errorCode = new StructuredQName("err", NamespaceConstant.ERR, "XPST0003")
    }
    val s = t.recentText(-1)
    val prefix = getLanguage + " syntax error " + (if (s.startsWith("...")) "near" else "in") + 
      ' ' + 
      Err.wrap(s) + 
      ":\n    "
    val err = new XPathException(message)
    err.setAdditionalLocationText(prefix)
    err.setIsStaticError(true)
    err.setErrorCodeQName(errorCode)
    throw err
  }

  /**
   * Set the current language (XPath or XQuery, XSLT Pattern, or SequenceType)
   * @param language one of the constants [[XPATH]], [[XSLT_PATTERN]], [[SEQUENCE_TYPE]]
   */
  def setLanguage(language: Int) = language match {
    case XPATH | XSLT_PATTERN | SEQUENCE_TYPE =>
    case _ => throw new IllegalArgumentException("Unknown language " + language)
  }

  /**
   * Get the current language (XPath or XQuery)
   * @return a string representation of the language being parsed, for use in error messages
   */
  protected def getLanguage(): String = language match {
    case XPATH => "XPath"
    case XSLT_PATTERN => "XSLT Pattern"
    case SEQUENCE_TYPE => "SequenceType"
    case _ => "XPath"
  }

  /**
   * Display the current token in an error message
   *
   * @return the display representation of the token
   */
  protected def currentTokenDisplay(): String = {
    if (t.currentToken == Token.NAME) {
      "name \"" + t.currentTokenValue + '\"'
    } else if (t.currentToken == Token.UNKNOWN) {
      "(unknown token)"
    } else {
      '\"' + Token.tokens(t.currentToken) + '\"'
    }
  }

  /**
   * Parse a string representing an expression. This will accept an XPath expression if called on an
   * ExpressionParser, or an XQuery expression if called on a QueryParser.
   *
   * @throws XPathException if the expression contains a syntax error
   * @param expression the expression expressed as a String
   * @param start offset within the string where parsing is to start
   * @param terminator character to treat as terminating the expression
   * @param env the static context for the expression
   * @return an Expression object representing the result of parsing
   */
  def parse(expression: String, 
      start: Int, 
      terminator: Int, 
      env: StaticContext): Expression = {
    this.env = env
    t = new Tokenizer()
    try {
      t.tokenize(expression, start, -1)
    } catch {
      case err: XPathException => grumble(err.getMessage)
    }
    val exp = parseExpression()
    if (t.currentToken != terminator) {
      if (t.currentToken == Token.EOF && terminator == Token.RCURLY) {
        grumble("Missing curly brace after expression in attribute value template", "XTSE0350")
      } else {
        //println(s"xxx `${t.currentToken}`")
        grumble("Unexpected token " + currentTokenDisplay() + " beyond end of expression")
      }
    }
    exp
  }

  /**
   * Parse a string representing a sequence type
   *
   * @param input the string, which should conform to the XPath SequenceType
   *      production
   * @param env the static context
   * @throws XPathException if any error is encountered
   * @return a SequenceType object representing the type
   */
  def parseSequenceType(input: String, env: StaticContext): SequenceType = {
    this.env = env
    language = SEQUENCE_TYPE
    t = new Tokenizer()
    try {
      t.tokenize(input, 0, -1)
    } catch {
      case err: XPathException => grumble(err.getMessage)
    }
    val req = parseSequenceType()
    if (t.currentToken != Token.EOF) {
      grumble("Unexpected token " + currentTokenDisplay() + " beyond end of SequenceType")
    }
    req
  }

  /**
   * Parse a top-level Expression:
   * ExprSingle ( ',' ExprSingle )*
   *
   * @throws XPathException if the expression contains a syntax error
   * @return the Expression object that results from parsing
   */
  def parseExpression(): Expression = {
    var exp = parseExprSingle()
    var list: ArrayList[Expression] = null
    while (t.currentToken == Token.COMMA) {
      if (list == null) {
        list = new ArrayList[Expression](10)
        list.add(exp)
      }
      nextToken()
      val next = parseExprSingle()
      setLocation(next)
      list.add(next)
    }
    if (list != null) {
      exp = Block.makeBlock(list)
      setLocation(exp)
    }
    exp
  }

  /**
   * Parse an ExprSingle
   *
   * @throws XPathException if any error is encountered
   * @return the resulting subexpression
   */
  def parseExprSingle(): Expression = t.currentToken match {
    case Token.FOR | Token.SOME | Token.EVERY => parseMappingExpression()
    case Token.IF => parseIfExpression()
    case _ => parseBinaryExpression(parseUnaryExpression(), 4)
  }

  /**
   * Parse a binary expression, using operator precedence parsing. This is used
   * to parse the part of the grammary consisting largely of binary operators
   * distinguished by precedence: from "or expressions" down to "unary expressions".
   * Algorithm for the mainstream binary operators is from Wikipedia article
   * on precedence parsing;  operator precedences are from the XQuery specification
   * appendix B.
   * @param _lhs Left-hand side "basic expression"
   */
  def parseBinaryExpression(_lhs: Expression, minPrecedence: Int): Expression = {
    var lhs = _lhs
    while (getCurrentOperatorPrecedence >= minPrecedence) {
      val operator = t.currentToken
      val prec = getCurrentOperatorPrecedence
      operator match {
        case Token.INSTANCE_OF | Token.TREAT_AS =>
          nextToken()
          val seq = parseSequenceType()
          lhs = makeSequenceTypeExpression(lhs, operator, seq)
          setLocation(lhs)
          if (getCurrentOperatorPrecedence >= prec) {
            grumble("Left operand of '" + Token.tokens(t.currentToken) + "' needs parentheses")
          }

        case Token.CAST_AS | Token.CASTABLE_AS =>
          nextToken()
          expect(Token.NAME)
          val at = getAtomicType(t.currentTokenValue)
          if (at == AtomicType.ANY_ATOMIC) {
            grumble("No value is castable to xs:anyAtomicType", "XPST0080")
          }
          nextToken()
          val allowEmpty = t.currentToken == Token.QMARK
          if (allowEmpty) {
            nextToken()
          }
          lhs = makeSingleTypeExpression(lhs, operator, at, allowEmpty)
          setLocation(lhs)
          if (getCurrentOperatorPrecedence >= prec) {
            grumble("Left operand of '" + Token.tokens(t.currentToken) + "' needs parentheses")
          }

        case _ =>
          nextToken()
          var rhs = parseUnaryExpression()
          while (getCurrentOperatorPrecedence > prec) {
            rhs = parseBinaryExpression(rhs, getCurrentOperatorPrecedence)
          }
          lhs = makeBinaryExpression(lhs, operator, rhs)
          setLocation(lhs)

      }
    }
    lhs
  }

  private def getCurrentOperatorPrecedence(): Int = t.currentToken match {
    case Token.OR => 4
    case Token.AND => 5
    case Token.FEQ | Token.FNE | Token.FLE | Token.FLT | Token.FGE | Token.FGT | Token.EQUALS | Token.NE | Token.LE | Token.LT | Token.GE | Token.GT | Token.IS | Token.PRECEDES | Token.FOLLOWS => 6
    case Token.TO => 7
    case Token.PLUS | Token.MINUS => 8
    case Token.MULT | Token.DIV | Token.IDIV | Token.MOD => 9
    case Token.UNION => 10
    case Token.INTERSECT | Token.EXCEPT => 11
    case Token.INSTANCE_OF => 12
    case Token.TREAT_AS => 13
    case Token.CASTABLE_AS => 14
    case Token.CAST_AS => 15
    case _ => -1
  }

  private def makeBinaryExpression(lhs: Expression, operator: Int, rhs: Expression): Expression = operator match {
    case Token.OR | Token.AND => new BooleanExpression(lhs, operator, rhs)
    case Token.FEQ | Token.FNE | Token.FLE | Token.FLT | Token.FGE | Token.FGT => new ValueComparison(lhs,
      operator, rhs)
    case Token.EQUALS | Token.NE | Token.LE | Token.LT | Token.GE | Token.GT => new GeneralComparison(lhs,
      operator, rhs)
    case Token.IS | Token.PRECEDES | Token.FOLLOWS => new IdentityComparison(lhs, operator, rhs)
    case Token.TO => new RangeExpression(lhs, operator, rhs)
    case Token.PLUS | Token.MINUS | Token.MULT | Token.DIV | Token.IDIV | Token.MOD => new ArithmeticExpression(lhs,
      operator, rhs)
    case Token.UNION | Token.INTERSECT | Token.EXCEPT => new VennExpression(lhs, operator, rhs)
    case _ => throw new IllegalArgumentException()
  }

  private def makeSequenceTypeExpression(lhs: Expression, operator: Int, `type`: SequenceType): Expression = operator match {
    case Token.INSTANCE_OF => new InstanceOfExpression(lhs, `type`)
    case Token.TREAT_AS =>
      val role = new RoleLocator(RoleLocator.TYPE_OP, "treat as", 0)
      role.setErrorCode("XPDY0050")
      val e = CardinalityChecker.makeCardinalityChecker(lhs, `type`.getCardinality, role)
      new ItemChecker(e, `type`.getPrimaryType, role)

    case _ => throw new IllegalArgumentException()
  }

  private def makeStructuredQName(lexicalName: String, defaultURI: String): StructuredQName = {
    StructuredQName.fromLexicalQName(lexicalName, defaultURI, env.getNamespaceResolver)
  }

  private def makeSingleTypeExpression(lhs: Expression, 
      operator: Int, 
      `type`: AtomicType, 
      allowEmpty: Boolean): Expression = {
    if (`type` == AtomicType.QNAME && lhs.isInstanceOf[StringLiteral]) {
      try {
        val source = lhs.asInstanceOf[StringLiteral].getStringValue
        makeStructuredQName(source, "")
        if (operator == Token.CASTABLE_AS) {
          return new Literal(BooleanValue.TRUE)
        } else {
          return new Literal(CastExpression.castStringToQName(source, env))
        }
      } catch {
        case e: XPathException => if (operator == Token.CASTABLE_AS) {
          return new Literal(BooleanValue.FALSE)
        } else {
          grumble(e.getMessage, e.getErrorCodeQName)
          return null
        }
      }
    }
    if (operator == Token.CASTABLE_AS) {
      new CastableExpression(lhs, `type`, allowEmpty)
    } else {
      new CastExpression(lhs, `type`, allowEmpty)
    }
  }

  /**
   * Parse a mapping expression. This is a common routine that handles
   * XPath 'for' expressions and quantified expressions.
   *
   * <p>Syntax: <br/>
   * (for|some|every) $x in expr (',' $y in expr)* (return|satisfies) expr
   * </p>
   *
   * <p>On entry, the current token indicates whether a for, some, or every
   * expression is expected.</p>
   *
   * @throws XPathException if any error is encountered
   * @return the resulting subexpression
   */
  private def parseMappingExpression(): Expression = {
    val offset = t.currentTokenStartOffset
    val operator = t.currentToken
    val clauseList = new ArrayList[ForClause](3)
    do {
      val clause = new ForClause()
      clause.offset = offset
      clause.requiredType = SequenceType.SINGLE_ITEM
      clauseList.add(clause)
      nextToken()
      skipToken(Token.DOLLAR)
      expect(Token.NAME)
      val `var` = t.currentTokenValue
      var v: Assignation = null
      if (operator == Token.FOR) {
        v = new ForExpression()
      } else {
        v = new QuantifiedExpression()
        v.asInstanceOf[QuantifiedExpression].setOperator(operator)
      }
      v.setVariableQName(makeStructuredQName(`var`, ""))
      clause.rangeVariable = v
      nextToken()
      skipToken(Token.IN)
      clause.sequence = parseExprSingle()
      declareRangeVariable(clause.rangeVariable)
    } while (t.currentToken == Token.COMMA)

    if (operator == Token.FOR) {
      skipToken(Token.RETURN)
    } else {
      skipToken(Token.SATISFIES)
    }
    var action = parseExprSingle()
    val th = TypeHierarchy.getInstance

    locally {
      var i = clauseList.size - 1
      while (i >= 0) {
        val fc = clauseList.get(i)
        val exp = fc.rangeVariable
        setLocation(exp)
        exp.setSequence(fc.sequence)
        val `type` = SequenceType.makeSequenceType(fc.sequence.getItemType, StaticProperty.EXACTLY_ONE)
        fc.rangeVariable.setRequiredType(`type`)
        exp.setAction(action)
        action = exp
        i -= 1
      }
    }
    locally {
      var i = clauseList.size - 1
      while (i >= 0) {
        val clause = clauseList.get(i)
        for (n <- 0 until clause.numberOfRangeVariables()) {
          undeclareRangeVariable()
        }
        i -= 1
      }
    }
    action
  }

  /**
   * Parse an IF expression:
   * if '(' expr ')' 'then' expr 'else' expr
   *
   * @throws XPathException if any error is encountered
   * @return the resulting subexpression
   */
  private def parseIfExpression(): Expression = {
    nextToken()
    val condition = parseExpression()
    skipToken(Token.RPAR)
    skipToken(Token.THEN)
    val thenExp = parseExprSingle()
    skipToken(Token.ELSE)
    val elseExp = parseExprSingle()
    val ifExp = Choose.makeConditional(condition, thenExp, elseExp)
    setLocation(ifExp)
    ifExp
  }

  /**
   * Analyze a token whose expected value is the name of an atomic type,
   * and return the object representing the atomic type.
   * @param qname The lexical QName of the atomic type; alternatively, a Clark name
   * @return The atomic type
   * @throws XPathException if the QName is invalid or if no atomic type of that
   * name exists as a built-in type or a type in an imported schema
   */
  private def getAtomicType(qname: String): AtomicType = {
    val name = makeStructuredQName(qname, env.getDefaultElementNamespace)
    if (name.getNamespaceURI == NamespaceConstant.SCHEMA) {
      val t = AtomicType.getSchemaType(name.getLocalName)
      if (t != null) {
        return t
      }
    }
    grumble("Unknown atomic type " + qname, "XPST0051")
    null
  }

  /**
   * Parse the sequence type production.
   * The QName must be the name of a built-in schema-defined data type.
   *
   * @throws XPathException if any error is encountered
   * @return the resulting subexpression
   */
  def parseSequenceType(): SequenceType = {
    val primaryType = parseItemType()
    if (primaryType.isInstanceOf[EmptySequenceTest]) {
      return SequenceType.makeSequenceType(primaryType, StaticProperty.EMPTY)
    }
    var occurrenceFlag: Int = 0
    t.currentToken match {
      case Token.STAR | Token.MULT => occurrenceFlag = StaticProperty.ALLOWS_ZERO_OR_MORE
      case Token.PLUS => occurrenceFlag = StaticProperty.ALLOWS_ONE_OR_MORE
      case Token.QMARK => occurrenceFlag = StaticProperty.ALLOWS_ZERO_OR_ONE
      case _ => return SequenceType.makeSequenceType(primaryType, StaticProperty.EXACTLY_ONE)
    }
    t.currentToken = Token.RPAR
    nextToken()
    SequenceType.makeSequenceType(primaryType, occurrenceFlag)
  }

  /**
   * Parse an ItemType within a SequenceType
   * @return the ItemType
   * @throws XPathException on a syntax error
   */
  protected def parseItemType(): ItemType = {
    var primaryType: ItemType = null
    if (t.currentToken == Token.NAME) {
      primaryType = getAtomicType(t.currentTokenValue)
      nextToken()
    } else if (t.currentToken == Token.NODEKIND) {
      if (t.currentTokenValue == "item") {
        nextToken()
        skipToken(Token.RPAR)
        primaryType = AnyItemType.getInstance
      } else if (t.currentTokenValue == "empty-sequence") {
        nextToken()
        skipToken(Token.RPAR)
        primaryType = EmptySequenceTest.getInstance
      } else {
        primaryType = parseKindTest()
      }
    } else {
      grumble("Expected type name in SequenceType, found " + Token.tokens(t.currentToken))
      return null
    }
    primaryType
  }

  /**
   * Parse a UnaryExpr:<br>
   * ('+'|'-')* ValueExpr
   * parsed as ('+'|'-')? UnaryExpr
   *
   * @throws XPathException if any error is encountered
   * @return the resulting subexpression
   */
  private def parseUnaryExpression(): Expression = {
    var exp: Expression = null
    t.currentToken match {
      case Token.MINUS =>
        nextToken()
        exp = new ArithmeticExpression(new Literal(IntegerValue.ZERO), Token.NEGATE, parseUnaryExpression())

      case Token.PLUS =>
        nextToken()
        exp = new ArithmeticExpression(new Literal(IntegerValue.ZERO), Token.PLUS, parseUnaryExpression())

      case _ => exp = parsePathExpression()
    }
    setLocation(exp)
    exp
  }

  /**
   * Test whether the current token is one that can start a RelativePathExpression
   *
   * @return the resulting subexpression
   */
  protected def atStartOfRelativePath(): Boolean = t.currentToken match {
    case Token.AXIS | Token.AT | Token.NAME | Token.PREFIX | Token.SUFFIX | Token.STAR | Token.NODEKIND | Token.DOT | Token.DOTDOT | Token.FUNCTION | Token.STRING_LITERAL | Token.NUMBER | Token.LPAR | Token.DOLLAR => true
    case _ => false
  }

  /**
   * Test whether the current token is one that is disallowed after a "leading lone slash".
   * These composite tokens have been parsed as operators, but are not allowed after "/" under the
   * rules of erratum E24
   *
   * @return the resulting subexpression
   */
  protected def disallowedAtStartOfRelativePath(): Boolean = t.currentToken match {
    case Token.CAST_AS | Token.CASTABLE_AS | Token.INSTANCE_OF | Token.TREAT_AS => true
    case _ => false
  }

  /**
   * Parse a PathExpresssion. This includes "true" path expressions such as A/B/C, and also
   * constructs that may start a path expression such as a variable reference $name or a
   * parenthesed expression (A|B). Numeric and string literals also come under this heading.
   *
   * @throws XPathException if any error is encountered
   * @return the resulting subexpression
   */
  protected def parsePathExpression(): Expression = t.currentToken match {
    case Token.SLASH =>
      nextToken()
      val start = new RootExpression()
      setLocation(start)
      if (disallowedAtStartOfRelativePath()) {
        grumble("Operator '" + Token.tokens(t.currentToken) + "' is not allowed after '/'")
      }
      if (atStartOfRelativePath()) {
        val path = parseRemainingPath(start)
        setLocation(path)
        path
      } else {
        start
      }

    case Token.SLSL =>
      nextToken()
      val start2 = new RootExpression()
      setLocation(start2)
      val axisExp = new AxisExpression(Axis.DESCENDANT_OR_SELF, null)
      setLocation(axisExp)
      val exp = parseRemainingPath(new SlashExpression(start2, axisExp))
      setLocation(exp)
      exp

    case _ => parseRelativePath()
  }

  /**
   * Parse a relative path (a sequence of steps). Called when the current token immediately
   * follows a separator (/ or //), or an implicit separator (XYZ is equivalent to ./XYZ)
   * @throws XPathException if any error is encountered
   * @return the resulting subexpression
   */
  protected def parseRelativePath(): Expression = {
    var exp = parseStepExpression(language == XSLT_PATTERN)
    while (t.currentToken == Token.SLASH || t.currentToken == Token.SLSL) {
      val op = t.currentToken
      nextToken()
      val next = parseStepExpression(false)
      if (op == Token.SLASH) {
        exp = new SlashExpression(exp, next)
      } else {
        val ae = new AxisExpression(Axis.DESCENDANT_OR_SELF, null)
        setLocation(ae)
        val se = new SlashExpression(ae, next)
        setLocation(se)
        exp = new SlashExpression(exp, se)
      }
      setLocation(exp)
    }
    exp
  }

  /**
   * Parse the remaining steps of an absolute path expression (one starting in "/" or "//"). Note that the
   * token immediately after the "/" or "//" has already been read, and in the case of "/", it has been confirmed
   * that we have a path expression starting with "/" rather than a standalone "/" expression.
   * @param start the initial implicit expression: root() in the case of "/", root()/descendant-or-self::node in
   * the case of "//"
   * @return the completed path expression
   * @throws XPathException
   */
  protected def parseRemainingPath(start: Expression): Expression = {
    var exp = start
    var op = Token.SLASH
    breakable {
      while (true) {
        val next = parseStepExpression(false)
        if (op == Token.SLASH) {
          exp = new SlashExpression(exp, next)
        } else {
          val descOrSelf = new AxisExpression(Axis.DESCENDANT_OR_SELF, null)
          setLocation(descOrSelf)
          val step = new SlashExpression(descOrSelf, next)
          setLocation(step)
          exp = new SlashExpression(exp, step)
        }
        setLocation(exp)
        op = t.currentToken
        if (op != Token.SLASH && op != Token.SLSL) {
          break()
        }
        nextToken()
      }
    }
    exp
  }

  /**
   * Parse a step (including an optional sequence of predicates)
   * @param firstInPattern true only if we are parsing the first step in a
   * RelativePathPattern in the XSLT Pattern syntax
   * @throws XPathException if any error is encountered
   * @return the resulting subexpression
   */
  protected def parseStepExpression(firstInPattern: Boolean): Expression = {
    var step = parseBasicStep(firstInPattern)
    val reverse = step.isInstanceOf[AxisExpression] &&
      !Axis.isForwards(step.asInstanceOf[AxisExpression].getAxis)
    breakable {
      while (true) {
        if (t.currentToken == Token.LSQB) {
          nextToken()
          val predicate = parsePredicate()
          skipToken(Token.RSQB)
          step = new FilterExpression(step, predicate)
          setLocation(step)
        } else {
          break()
        }
      }
    }
    if (reverse) {
      SystemFunction.makeSystemFunction("reverse", Array(step))
    } else {
      step
    }
  }

  /**
   * Parse the expression within a predicate. A separate method so it can be overridden
   */
  protected def parsePredicate(): Expression = parseExpression()

  /**
   * Parse a basic step expression (without the predicates)
   *
   * @throws XPathException if any error is encountered
   * @return the resulting subexpression
   * @param firstInPattern true only if we are parsing the first step in a
   * RelativePathPattern in the XSLT Pattern syntax
   */
  protected def parseBasicStep(firstInPattern: Boolean): Expression = t.currentToken match {
    case Token.DOLLAR => parseVariableReference()
    case Token.LPAR =>
      nextToken()
      if (t.currentToken == Token.RPAR) {
        nextToken()
        new Literal(EmptySequence.getInstance)
      }
      val seq = parseExpression()
      skipToken(Token.RPAR)
      seq

    case Token.STRING_LITERAL => parseStringLiteral()
    case Token.NUMBER => parseNumericLiteral()
    case Token.FUNCTION => parseFunctionCall()
    case Token.DOT =>
      nextToken()
      val cie = new ContextItemExpression()
      setLocation(cie)
      cie

    case Token.DOTDOT =>
      nextToken()
      val pne = new ParentNodeExpression()
      setLocation(pne)
      pne

    case Token.NODEKIND | Token.NAME | Token.PREFIX | Token.SUFFIX | Token.STAR =>
      var defaultAxis = Axis.CHILD
      if (t.currentToken == Token.NODEKIND && 
        (t.currentTokenValue == "attribute" || t.currentTokenValue == "schema-attribute")) {
        defaultAxis = Axis.ATTRIBUTE
      } else if (firstInPattern && t.currentToken == Token.NODEKIND && t.currentTokenValue == "document-node") {
        defaultAxis = Axis.SELF
      }
      var test = parseNodeTest(Type.ELEMENT)
      if (test.isInstanceOf[AnyNodeTest]) {
        test = if (defaultAxis == Axis.CHILD) AnyChildNodeTest.getInstance else NodeKindTest.ATTRIBUTE
      }
      val ae = new AxisExpression(defaultAxis, test)
      setLocation(ae)
      ae

    case Token.AT =>
      nextToken()
      t.currentToken match {
        case Token.NAME | Token.PREFIX | Token.SUFFIX | Token.STAR | Token.NODEKIND =>
          val ae2 = new AxisExpression(Axis.ATTRIBUTE, parseNodeTest(Type.ATTRIBUTE))
          setLocation(ae2)
          ae2

        case _ => grumble("@ must be followed by a NodeTest")
      }

    case Token.AXIS =>
      var axis: Byte = 0
      try {
        axis = Axis.getAxisNumber(t.currentTokenValue)
      } catch {
        case err: XPathException => {
          grumble(err.getMessage)
          axis = Axis.CHILD
        }
      }
      val principalNodeType = Axis.principalNodeType(axis)
      nextToken()
      t.currentToken match {
        case Token.NAME | Token.PREFIX | Token.SUFFIX | Token.STAR | Token.NODEKIND =>
          val ax = new AxisExpression(axis, parseNodeTest(principalNodeType))
          setLocation(ax)
          ax

        case _ => grumble("Unexpected token " + currentTokenDisplay() + " after axis name")
      }

    case _ => grumble("Unexpected token " + currentTokenDisplay() + " in path expression")
  }

  protected def parseNumericLiteral(): Expression = {
    val number = NumericValue.parseNumber(t.currentTokenValue)
    if (number.isNaN) {
      grumble("Invalid numeric literal " + Err.wrap(t.currentTokenValue, Err.VALUE))
    }
    nextToken()
    val lit = new Literal(number)
    setLocation(lit)
    lit
  }

  protected def parseStringLiteral(): Expression = {
    val literal = makeStringLiteral(t.currentTokenValue)
    nextToken()
    literal
  }

  protected def parseVariableReference(): Expression = {
    nextToken()
    val `var` = t.currentTokenValue
    skipToken(Token.NAME)
    val vtest = makeStructuredQName(`var`, "")
    val b = findRangeVariable(vtest)
    var ref: Expression = null
    if (b != null) {
      ref = new LocalVariableReference(b)
    } else {
      try {
        ref = env.bindVariable(vtest)
      } catch {
        case err: XPathException => if ("XPST0008" == err.getErrorCodeLocalPart) {
          grumble("Variable $" + `var` + " has not been declared", "XPST0008")
          return null
        } else {
          throw err
        }
      }
    }
    setLocation(ref)
    ref
  }

  /**
   * Method to make a string literal from a token identified as a string
   * literal. This is trivial in XPath, but in XQuery the method is overridden
   * to identify pseudo-XML character and entity references. Note that the job of handling
   * doubled string delimiters is done by the tokenizer.
   * @param currentTokenValue the token as read (excluding quotation marks)
   * @return The string value of the string literal
   */
  private def makeStringLiteral(currentTokenValue: String): Literal = {
    val literal = new StringLiteral(currentTokenValue)
    setLocation(literal)
    literal
  }

  /**
   * Parse a NodeTest.
   * One of QName, prefix:*, *:suffix, *, text(), node(), comment(), or
   * processing-instruction(literal?), or element(~,~), attribute(~,~), etc.
   *
   * @throws XPathException if any error is encountered
   * @param nodeType the node type being sought if one is specified
   * @return the resulting NodeTest object
   */
  private def parseNodeTest(nodeType: Short): NodeTest = {
    val tok = t.currentToken
    var tokv = t.currentTokenValue
    tok match {
      case Token.NAME =>
        nextToken()
        val nameCode = makeStructuredQName(tokv, if (nodeType == Type.ELEMENT) env.getDefaultElementNamespace else "")
        new NameTest(nodeType, nameCode)

      case Token.PREFIX =>
        nextToken()
        makeNamespaceTest(nodeType, tokv)

      case Token.SUFFIX =>
        nextToken()
        tokv = t.currentTokenValue
        skipToken(Token.NAME)
        makeLocalNameTest(nodeType, tokv)

      case Token.STAR =>
        nextToken()
        NodeKindTest.makeNodeKindTest(nodeType)

      case Token.NODEKIND => parseKindTest()
      case _ =>
        grumble("Unrecognized node test")
        null

    }
  }

  /**
   * Parse a KindTest
   * @return the KindTest, expressed as a NodeTest object
   * @throws XPathException to indicate a syntax error
   */
  private def parseKindTest(): NodeTest = {
    val typeName = t.currentTokenValue
    if (typeName.startsWith("schema-")) {
      grumble(typeName + "() requires an imported schema")
      return null
    }
    val primaryType = getSystemType(typeName)
    var empty = false
    nextToken()
    if (t.currentToken == Token.RPAR) {
      empty = true
    }
    var result: NodeTest = null
    primaryType match {
      case Type.ITEM =>
        grumble("item() is not allowed in a path expression")
        return null

      case Type.NODE => result = AnyNodeTest.getInstance
      case Type.TEXT => result = NodeKindTest.TEXT
      case Type.COMMENT => result = NodeKindTest.COMMENT
      case Type.DOCUMENT =>
        result = NodeKindTest.DOCUMENT
        if (!empty) {
          val innerType = getSystemType(t.currentTokenValue)
          if (innerType != Type.ELEMENT) {
            grumble("Argument to document-node() must be an element type")
          }
          val inner = parseKindTest()
          skipToken(Token.RPAR)
          return new DocumentNodeTest(inner)
        }

      case Type.PROCESSING_INSTRUCTION =>
        result = NodeKindTest.PROCESSING_INSTRUCTION
        if (!empty) {
          var piName: StructuredQName = null
          if (t.currentToken == Token.STRING_LITERAL) {
            t.currentToken = Token.NAME
            t.currentTokenValue = Whitespace.trim(t.currentTokenValue)
          }
          if (t.currentToken == Token.NAME && NameChecker.isValidNCName(t.currentTokenValue)) {
            piName = new StructuredQName("", "", t.currentTokenValue)
          }
          if (piName == null) {
            grumble("Processing instruction name must be an NCName (optionally in quotes)")
          }
          nextToken()
          skipToken(Token.RPAR)
          return new NameTest(Type.PROCESSING_INSTRUCTION, piName)
        }

      case Type.ATTRIBUTE | Type.ELEMENT =>
        result = NodeKindTest.makeNodeKindTest(primaryType)
        if (!empty) {
          if (t.currentToken == Token.STAR || t.currentToken == Token.MULT) {
          } else if (t.currentToken == Token.NAME) {
            val nodeQName = makeStructuredQName(t.currentTokenValue, if (primaryType == Type.ELEMENT) env.getDefaultElementNamespace else "")
            result = new NameTest(primaryType, nodeQName)
          } else {
            grumble("Unexpected " + Token.tokens(t.currentToken) + " in SequenceType")
          }
          nextToken()
          if (t.currentToken == Token.COMMA) {
            nextToken()
            val contentType = makeStructuredQName(t.currentTokenValue, env.getDefaultElementNamespace)
            var recognized = false
            if (contentType.getNamespaceURI == NamespaceConstant.SCHEMA) {
              val local = contentType.getLocalName
              recognized = AtomicType.isRecognizedName(local)
              if (primaryType == Type.ELEMENT && !(local == "anyType" || local == "untyped")) {
                result = EmptySequenceTest.getInstance
              }
              if (primaryType == Type.ATTRIBUTE && 
                !(local == "anyAtomicType" || local == "untypedAtomic")) {
                result = EmptySequenceTest.getInstance
              }
            }
            if (!recognized) {
              grumble("Unknown type name: " + t.currentTokenValue)
            }
            nextToken()
            if (primaryType == Type.ELEMENT && t.currentToken == Token.QMARK) {
              nextToken()
            }
          }
          skipToken(Token.RPAR)
          return result
        }

      case _ =>
        grumble("Unknown node kind")
        return null

    }
    if (!empty) {
      grumble("Kind test " + typeName + "() must be empty")
    }
    skipToken(Token.RPAR)
    result
  }

  /**
   * Expect a specific token at the current position, and move to the next token
   * @param expected the expected token
   * @throws XPathException if the expected token was not found
   */
  private def skipToken(expected: Int): Unit = {
    expect(expected)
    nextToken()
  }

  /**
   * Get a system type - that is, one whose name is a keyword rather than a QName. This includes the node
   * kinds such as element and attribute, and the generic types node() and item()
   *
   * @param name the name of the system type, for example "element" or "comment"
   * @return the integer constant denoting the type, for example [[Type.ITEM]] or [[Type.ELEMENT]]
   * @throws XPathException if the name is not recognized
   */
  private def getSystemType(name: String): Int = {
    if ("item" == name) Type.ITEM else if ("document-node" == name) Type.DOCUMENT else if ("element" == name) Type.ELEMENT else if ("attribute" == name) Type.ATTRIBUTE else if ("text" == name) Type.TEXT else if ("comment" == name) Type.COMMENT else if ("processing-instruction" == name) Type.PROCESSING_INSTRUCTION else if ("node" == name) Type.NODE else {
      grumble("Unknown type " + name)
      -1
    }
  }

  /**
   * Parse a function call.
   * function-name '(' ( Expression (',' Expression )* )? ')'
   *
   * @throws XPathException if any error is encountered
   * @return the resulting subexpression
   */
  protected def parseFunctionCall(): Expression = {
    val fname = t.currentTokenValue
    val args = new ArrayList[Expression](10)
    val functionName = makeStructuredQName(fname, env.getDefaultFunctionNamespace)
    nextToken()
    if (t.currentToken != Token.RPAR) {
      breakable {
        while (true) {
          val arg = parseFunctionArgument()
          args.add(arg)
          if (t.currentToken == Token.COMMA) {
            nextToken()
          } else {
            break()
          }
        }
      }
      expect(Token.RPAR)
    }
    nextToken()
    val arguments = new Array[Expression](args.size)
    args.toArray(arguments)
    var fcall: Expression = null
    try {
      fcall = env.getFunctionLibrary.bind(functionName, arguments, env, defaultContainer)
    } catch {
      case err: XPathException => {
        if (err.getErrorCodeQName == null) {
          err.setErrorCode("XPST0017")
          err.setIsStaticError(true)
        }
        grumble(err.getMessage, err.getErrorCodeQName)
        return null
      }
    }
    if (fcall == null) {
      val msg = "Cannot find a matching " + arguments.length + "-argument function named " + 
        functionName.getClarkName + 
        "()"
      if (env.isInBackwardsCompatibleMode) {
        val err = new XPathException(msg, "XTDE1425")
        val exp = new ErrorExpression(err)
        setLocation(exp)
        return exp
      }
      grumble(msg, "XPST0017")
      return null
    }
    if (fcall.isInstanceOf[CastExpression] && fcall.getItemType == AtomicType.QNAME && 
      arguments(0).isInstanceOf[StringLiteral]) {
      try {
        val av = CastExpression.castStringToQName(arguments(0).asInstanceOf[StringLiteral].getStringValue, 
          env)
        return new Literal(av)
      } catch {
        case e: XPathException =>
          grumble(e.getMessage, e.getErrorCodeQName)
          return null
      }
    }
//ORBEON XSLT
//    if (language == XSLT_PATTERN) {
//      if (fcall.isInstanceOf[RegexGroup]) {
//        return new Literal(EmptySequence.getInstance)
//      } else if (fcall.isInstanceOf[CurrentGroup]) {
//        grumble("The current-group() function cannot be used in a pattern", "XTSE1060")
//        return null
//      } else if (fcall.isInstanceOf[CurrentGroupingKey]) {
//        grumble("The current-grouping-key() function cannot be used in a pattern", "XTSE1070")
//        return null
//      }
//    }
    setLocation(fcall)
    for (argument <- arguments) {
      fcall.adoptChildExpression(argument)
    }
    fcall
  }

  /**
   * Parse an argument to a function call. Separate method so it can
   * be overridden.
   * @return the argument expression
   * @throws XPathException if there is a syntax error
   */
  protected def parseFunctionArgument(): Expression = parseExprSingle()

  /**
   * Declare a range variable (record its existence within the parser).
   * A range variable is a variable declared within an expression, as distinct
   * from a variable declared in the context.
   *
   * @param declaration the variable declaration to be added to the stack
   * @throws XPathException if any error is encountered
   */
  def declareRangeVariable(declaration: Binding): Unit = {
    rangeVariables.push(declaration)
  }

  /**
   * Note when the most recently declared range variable has gone out of scope
   */
  def undeclareRangeVariable(): Unit = {
    rangeVariables.pop()
  }

  /**
   * Locate a range variable with a given name. (By "range variable", we mean a
   * variable declared within the expression where it is used.)
   *
   * @param qName identifies the name of the range variable
   * @return null if not found (this means the variable is probably a
   *     context variable); otherwise the relevant RangeVariable
   */
  protected def findRangeVariable(qName: StructuredQName): Binding = {
    var v = rangeVariables.size - 1
    while (v >= 0) {
      val b = rangeVariables.get(v)
      if (b.getVariableQName == qName) {
        return b
      }
      v -= 1
    }
    null
  }

  /**
   * Make a NamespaceTest (name:*)
   *
   * @param nodeType integer code identifying the type of node required
   * @param prefix the namespace prefix
   * @throws XPathException if the namespace prefix is not declared
   * @return the NamespaceTest, a pattern that matches all nodes in this
   *     namespace
   */
  def makeNamespaceTest(nodeType: Short, prefix: String): NamespaceTest = {
    var uri: String = null
    try {
      val qn = makeStructuredQName(prefix + ":a", "")
      uri = qn.getNamespaceURI
    } catch {
      case e: XPathException =>
        grumble(e.getMessage, "XPST0081")
        return null
    }
    new NamespaceTest(nodeType, uri)
  }

  /**
   * Make a LocalNameTest (*:name)
   *
   * @param nodeType the kind of node to be matched
   * @param localName the requred local name
   * @throws XPathException if the local name is invalid
   * @return a LocalNameTest, a pattern which matches all nodes of a given
   *     local name, regardless of namespace
   */
  def makeLocalNameTest(nodeType: Short, localName: String): LocalNameTest = {
    if (!NameChecker.isValidNCName(localName)) {
      grumble("Local name [" + localName + "] contains invalid characters")
    }
    new LocalNameTest(nodeType, localName)
  }

  /**
   * Set location information on an expression. At present only the line number
   * is retained. Needed mainly for XQuery.
   * @param exp the expression whose location information is to be set
   */
  def setLocation(exp: Expression): Unit = {
    if (exp.getContainer == null) {
      exp.setContainer(defaultContainer)
    }
  }
}
