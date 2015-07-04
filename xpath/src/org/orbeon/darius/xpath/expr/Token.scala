// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr

import java.{util ⇒ ju}

object Token {

  /**
   * Pseudo-token representing the end of the expression
   */
  val EOF = 0

  /**
   * "union" or "|" token
   */
  val UNION = 1

  /**
   * Forwards "/"
   */
  val SLASH = 2

  /**
   * At token, "@"
   */
  val AT = 3

  /**
   * Left square bracket
   */
  val LSQB = 4

  /**
   * Left parenthesis
   */
  val LPAR = 5

  /**
   * Equals token ("=")
   */
  val EQUALS = 6

  /**
   * Comma token
   */
  val COMMA = 7

  /**
   * Double forwards slash, "//"
   */
  val SLSL = 8

  /**
   * Operator "or"
   */
  val OR = 9

  /**
   * Operator "and"
   */
  val AND = 10

  /**
   * Operator ">"
   */
  val GT = 11

  /**
   * Operator "<"
   */
  val LT = 12

  /**
   * Operator ">="
   */
  val GE = 13

  /**
   * Operator "<="
   */
  val LE = 14

  /**
   * Operator "+"
   */
  val PLUS = 15

  /**
   * Binary minus operator
   */
  val MINUS = 16

  /**
   * Multiply operator, "*" when used in an operator context
   */
  val MULT = 17

  /**
   * Operator "div"
   */
  val DIV = 18

  /**
   * Operator "mod"
   */
  val MOD = 19

  /**
   * Operator "is"
   */
  val IS = 20

  /**
   * "$" symbol
   */
  val DOLLAR = 21

  /**
   * Operator not-equals. That is, "!="
   */
  val NE = 22

  /**
   * Operator "intersect"
   */
  val INTERSECT = 23

  /**
   * Operator "except"
   */
  val EXCEPT = 24

  /**
   * Keyword "return"
   */
  val RETURN = 25

  /**
   * Ketword "then"
   */
  val THEN = 26

  /**
   * Keyword "else"
   */
  val ELSE = 27

  /**
   * Operator "to"
   */
  val TO = 29

  /**
   * Keyword "in"
   */
  val IN = 30

  /**
   * Keyword "some"
   */
  val SOME = 31

  /**
   * Keyword "every"
   */
  val EVERY = 32

  /**
   * Keyword "satisfies"
   */
  val SATISFIES = 33

  /**
   * Token representing the name of a function and the following "(" symbol
   */
  val FUNCTION = 34

  /**
   * Token representing the name of an axis and the following "::" symbol
   */
  val AXIS = 35

  /**
   * Keyword "if"
   */
  val IF = 36

  /**
   * Operator "<<"
   */
  val PRECEDES = 37

  /**
   * Operator ">>"
   */
  val FOLLOWS = 38

  /**
   * "::" symbol
   */
  val COLONCOLON = 39

  /**
   * ":*" symbol
   */
  val COLONSTAR = 40

  /**
   * # symbol
   */
  val HASH = 44

  /**
   * operator "instance of"
   */
  val INSTANCE_OF = 45

  /**
   * operator "cast as"
   */
  val CAST_AS = 46

  /**
   * operator "treat as"
   */
  val TREAT_AS = 47

  /**
   * operator "eq"
   */
  val FEQ = 50

  /**
   * operator "ne"
   */
  val FNE = 51

  /**
   * operator "gt"
   */
  val FGT = 52

  /**
   * operator "lt"
   */
  val FLT = 53

  /**
   * operator "ge"
   */
  val FGE = 54

  /**
   * opeartor "le"
   */
  val FLE = 55

  /**
   * operator "idiv"
   */
  val IDIV = 56

  /**
   * operator "castable as"
   */
  val CASTABLE_AS = 57

  /**
   * Node kind, e.g. "node()" or "comment()"
   */
  val NODEKIND = 69

  /**
   * "*:" token
   */
  val SUFFIX = 70

  /**
   * "as" (in XQuery Update rename expression)
   */
  val AS = 71

  /**
   * Constant identifying the token number of the last token to be classified as an operator
   */
  var LAST_OPERATOR: Int = 150

  /**
   * Name token (a QName, in general)
   */
  val NAME = 201

  /**
   * String literal
   */
  val STRING_LITERAL = 202

  /**
   * Right square bracket
   */
  val RSQB = 203

  /**
   * Right parenthesis
   */
  val RPAR = 204

  /**
   * "." symbol
   */
  val DOT = 205

  /**
   * ".." symbol
   */
  val DOTDOT = 206

  /**
   * "*" symbol when used as a wildcard
   */
  val STAR = 207

  /**
   * "prefix:*" token
   */
  val PREFIX = 208

  /**
   * Numeric literal
   */
  val NUMBER = 209

  /**
   * "for" keyword
   */
  val FOR = 211

  /**
   * Question mark symbol. That is, "?"
   */
  val QMARK = 213

  /**
   * "}" symbol (XQuery only)
   */
  val RCURLY = 215

  /**
   * Unary minus sign
   */
  val NEGATE = 299

  /**
   * The following strings are used to represent tokens in error messages
   */
  val tokens = new Array[String](300)

  tokens(EOF) = "<eof>"

  tokens(UNION) = "|"

  tokens(SLASH) = "/"

  tokens(AT) = "@"

  tokens(LSQB) = "["

  tokens(LPAR) = "("

  tokens(EQUALS) = "="

  tokens(COMMA) = ","

  tokens(SLSL) = "//"

  tokens(OR) = "or"

  tokens(AND) = "and"

  tokens(GT) = ">"

  tokens(LT) = "<"

  tokens(GE) = ">="

  tokens(LE) = "<="

  tokens(PLUS) = "+"

  tokens(MINUS) = "-"

  tokens(MULT) = "*"

  tokens(DIV) = "div"

  tokens(MOD) = "mod"

  tokens(IS) = "is"

  tokens(DOLLAR) = "$"

  tokens(NE) = "!="

  tokens(INTERSECT) = "intersect"

  tokens(EXCEPT) = "except"

  tokens(RETURN) = "return"

  tokens(THEN) = "then"

  tokens(ELSE) = "else"

  tokens(TO) = "to"

  tokens(IN) = "in"

  tokens(SOME) = "some"

  tokens(EVERY) = "every"

  tokens(SATISFIES) = "satisfies"

  tokens(FUNCTION) = "<function>("

  tokens(AXIS) = "<axis>"

  tokens(IF) = "if("

  tokens(PRECEDES) = "<<"

  tokens(FOLLOWS) = ">>"

  tokens(COLONCOLON) = "::"

  tokens(COLONSTAR) = ":*"

  tokens(HASH) = "#"

  tokens(INSTANCE_OF) = "instance of"

  tokens(CAST_AS) = "cast as"

  tokens(TREAT_AS) = "treat as"

  tokens(FEQ) = "eq"

  tokens(FNE) = "ne"

  tokens(FGT) = "gt"

  tokens(FGE) = "ge"

  tokens(FLT) = "lt"

  tokens(FLE) = "le"

  tokens(IDIV) = "idiv"

  tokens(CASTABLE_AS) = "castable as"

  tokens(AS) = "as"

  tokens(NAME) = "<name>"

  tokens(STRING_LITERAL) = "<string-literal>"

  tokens(RSQB) = "]"

  tokens(RPAR) = ")"

  tokens(DOT) = "."

  tokens(DOTDOT) = ".."

  tokens(STAR) = "*"

  tokens(PREFIX) = "<prefix:*>"

  tokens(NUMBER) = "<numeric-literal>"

  tokens(NODEKIND) = "<node-type>()"

  tokens(FOR) = "for"

  tokens(SUFFIX) = "<*:local-name>"

  tokens(RCURLY) = "}"

  tokens(NEGATE) = "-"

  /**
   * Lookup table for composite (two-keyword) tokens
   */
  var doubleKeywords: ju.HashMap[String, Integer] = new ju.HashMap[String, Integer](30)

  /**
   * Pseudo-token representing the start of the expression
   */
  val UNKNOWN = -1

  mapDouble("instance of", INSTANCE_OF)

  mapDouble("cast as", CAST_AS)

  mapDouble("treat as", TREAT_AS)

  mapDouble("castable as", CASTABLE_AS)

  private def mapDouble(doubleKeyword: String, token: Int): Unit = {
    doubleKeywords.put(doubleKeyword, new java.lang.Integer(token))
    tokens(token) = doubleKeyword
  }

  /**
   * Return the inverse of a relational operator, so that "a op b" can be
   * rewritten as "b inverse(op) a"
   */
  def inverse(operator: Int): Int = operator match {
    case LT ⇒ GT
    case LE ⇒ GE
    case GT ⇒ LT
    case GE ⇒ LE
    case FLT ⇒ FGT
    case FLE ⇒ FGE
    case FGT ⇒ FLT
    case FGE ⇒ FLE
    case _ ⇒ operator
  }

  /**
   * Return the negation of a relational operator, so that "a op b" can be
   * rewritten as not(b op' a)
   */
  def negate(operator: Int): Int = operator match {
    case FEQ ⇒ FNE
    case FNE ⇒ FEQ
    case FLT ⇒ FGE
    case FLE ⇒ FGT
    case FGT ⇒ FLE
    case FGE ⇒ FLT
    case _ ⇒ throw new IllegalArgumentException("Invalid operator for negate()")
  }

  def isOrderedOperator(operator: Int): Boolean = operator != FEQ && operator != FNE
}
