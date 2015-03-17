package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.Whitespace
import Tokenizer._
//remove if not needed
import scala.collection.JavaConversions._

object Tokenizer {

  /**
   * Initial default state of the Tokenizer
   */
  val DEFAULT_STATE = 0

  /**
   * State in which a name is NOT to be merged with what comes next, for example "("
   */
  val BARE_NAME_STATE = 1

  /**
   * Identify a binary operator
   *
   * @param s String representation of the operator - must be interned
   * @return the token number of the operator, or UNKNOWN if it is not a
   *     known operator
   */
  private def getBinaryOp(s: String): Int = s.length match {
    case 2 => 
      if (s == "or") Token.OR
      if (s == "is") Token.IS
      if (s == "to") Token.TO
      if (s == "in") Token.IN
      if (s == "eq") Token.FEQ
      if (s == "ne") Token.FNE
      if (s == "gt") Token.FGT
      if (s == "ge") Token.FGE
      if (s == "lt") Token.FLT
      if (s == "le") Token.FLE
      if (s == "as") Token.AS

    case 3 => 
      if (s == "and") Token.AND
      if (s == "div") Token.DIV
      if (s == "mod") Token.MOD

    case 4 => 
      if (s == "idiv") Token.IDIV
      if (s == "then") Token.THEN
      if (s == "else") Token.ELSE

    case 5 => if (s == "union") Token.UNION
    case 6 => 
      if (s == "except") Token.EXCEPT
      if (s == "return") Token.RETURN

    case 9 => 
      if (s == "intersect") Token.INTERSECT
      if (s == "satisfies") Token.SATISFIES

  }

  /**
   * Distinguish nodekind names, "if", and function names, which are all
   * followed by a "("
   *
   * @param s the name - must be interned
   * @return the token number
   */
  private def getFunctionType(s: String): Int = s.length match {
    case 2 => if (s == "if") Token.IF
    case 4 => 
      if (s == "node") Token.NODEKIND
      if (s == "item") Token.NODEKIND
      if (s == "text") Token.NODEKIND

    case 7 => 
      if (s == "element") Token.NODEKIND
      if (s == "comment") Token.NODEKIND

    case 9 => if (s == "attribute") Token.NODEKIND
    case _ => 
      if (s == "document-node") Token.NODEKIND
      if (s == "empty-sequence") Token.NODEKIND
      if (s == "schema-element") Token.NODEKIND
      if (s == "schema-attribute") Token.NODEKIND
      if (s == "processing-instruction") Token.NODEKIND

  }
}

/**
 * Tokenizer for expressions and inputs.
 *
 * This code was originally derived from James Clark's xt, but has been almost entirely rewritten.
 */
class Tokenizer {

  private var state: Int = DEFAULT_STATE

  /**
   * The number identifying the most recently read token
   */
  var currentToken: Int = Token.EOF

  /**
   * The string value of the most recently read token
   */
  var currentTokenValue: String = null

  /**
   * The position in the input expression where the current token starts
   */
  var currentTokenStartOffset: Int = 0

  /**
   * The number of the next token to be returned
   */
  private var nextToken: Int = Token.EOF

  /**
   * The string value of the next token to be returned
   */
  private var nextTokenValue: String = null

  /**
   * The position in the expression of the start of the next token
   */
  private var nextTokenStartOffset: Int = 0

  /**
   * The string being parsed
   */
  var input: String = _

  /**
   * The current position within the input string
   */
  var inputOffset: Int = 0

  /**
   * The length of the input string
   */
  private var inputLength: Int = _

  /**
   * The token number of the token that preceded the current token
   */
  private var precedingToken: Int = Token.UNKNOWN

  /**
   * Prepare a string for tokenization.
   * The actual tokens are obtained by calls on next()
   *
   * @param input the string to be tokenized
   * @param start start point within the string
   * @param end end point within the string (last character not read):
   * -1 means end of string
   * @throws XPathException if a lexical error occurs, e.g. unmatched
   *     string quotes
   */
  def tokenize(input: String, start: Int, end: Int) {
    nextToken = Token.EOF
    nextTokenValue = null
    nextTokenStartOffset = 0
    inputOffset = start
    this.input = input
    inputLength = if (end == -1) input.length else end
    lookAhead()
    next()
  }

  /**
   * Get the next token from the input expression. The type of token is returned in the
   * currentToken variable, the string value of the token in currentTokenValue.
   *
   * @throws XPathException if a lexical error is detected
   */
  def next() {
    precedingToken = currentToken
    currentToken = nextToken
    currentTokenValue = nextTokenValue
    if (currentTokenValue == null) {
      currentTokenValue = ""
    }
    currentTokenStartOffset = nextTokenStartOffset
    currentToken match {
      case Token.NAME => 
        var optype = getBinaryOp(currentTokenValue)
        if (optype != Token.UNKNOWN && !followsOperator(precedingToken)) {
          currentToken = optype
        }

      case Token.STAR => if (!followsOperator(precedingToken)) {
        currentToken = Token.MULT
      }
    }
    if (currentToken == Token.RCURLY) {
      return
    }
    val oldPrecedingToken = precedingToken
    lookAhead()
    if (currentToken == Token.NAME) {
      if (state == BARE_NAME_STATE) {
        return
      }
      if (oldPrecedingToken == Token.DOLLAR) {
        return
      }
      nextToken match {
        case Token.LPAR => 
          var op = getBinaryOp(currentTokenValue)
          if (op == Token.UNKNOWN || followsOperator(oldPrecedingToken)) {
            currentToken = getFunctionType(currentTokenValue)
            lookAhead()
          } else {
            currentToken = op
          }

        case Token.COLONCOLON => 
          lookAhead()
          currentToken = Token.AXIS

        case Token.COLONSTAR => 
          lookAhead()
          currentToken = Token.PREFIX

        case Token.DOLLAR => if (currentTokenValue == "for") {
          currentToken = Token.FOR
        } else if (currentTokenValue == "some") {
          currentToken = Token.SOME
        } else if (currentTokenValue == "every") {
          currentToken = Token.EVERY
        }
        case Token.NAME => 
          var composite = currentTokenValue + ' ' + nextTokenValue
          var `val` = Token.doubleKeywords.get(composite)
          if (`val` == null) {
            //break
          } else {
            currentToken = `val`
            currentTokenValue = composite
            lookAhead()
            return
          }

        case _}
    }
  }

  /**
   * Look ahead by one token. This method does the real tokenization work.
   * The method is normally called internally, but the XQuery parser also
   * calls it to resume normal tokenization after dealing with pseudo-XML
   * syntax.
   * @throws XPathException if a lexical error occurs
   */
  def lookAhead() {
    precedingToken = nextToken
    nextTokenValue = null
    nextTokenStartOffset = inputOffset
    while (true) {
      if (inputOffset >= inputLength) {
        nextToken = Token.EOF
        return
      }
      var c = input.charAt(inputOffset += 1)
      c match {
        case '/' => 
          if (inputOffset < inputLength && input.charAt(inputOffset) == '/') {
            inputOffset += 1
            nextToken = Token.SLSL
            return
          }
          nextToken = Token.SLASH
          return

        case ':' => 
          if (inputOffset < inputLength) {
            if (input.charAt(inputOffset) == ':') {
              inputOffset += 1
              nextToken = Token.COLONCOLON
              return
            }
          }
          throw new XPathException("Unexpected colon at start of token")

        case '@' => 
          nextToken = Token.AT
          return

        case '?' => 
          nextToken = Token.QMARK
          return

        case '[' => 
          nextToken = Token.LSQB
          return

        case ']' => 
          nextToken = Token.RSQB
          return

        case '}' => 
          nextToken = Token.RCURLY
          return

        case '(' => 
          if (inputOffset < inputLength && input.charAt(inputOffset) == ':') {
            inputOffset += 1
            var nestingDepth = 1
            while (nestingDepth > 0 && inputOffset < (inputLength - 1)) {
              if (input.charAt(inputOffset) == ':' && input.charAt(inputOffset + 1) == ')') {
                nestingDepth -= 1
                inputOffset += 1
              } else if (input.charAt(inputOffset) == '(' && input.charAt(inputOffset + 1) == ':') {
                nestingDepth += 1
                inputOffset += 1
              }
              inputOffset += 1
            }
            if (nestingDepth > 0) {
              throw new XPathException("Unclosed XPath comment")
            }
            lookAhead()
          } else {
            nextToken = Token.LPAR
          }
          return

        case ')' => 
          nextToken = Token.RPAR
          return

        case '+' => 
          nextToken = Token.PLUS
          return

        case '-' => 
          nextToken = Token.MINUS
          return

        case '=' => 
          nextToken = Token.EQUALS
          return

        case '!' => 
          if (inputOffset < inputLength && input.charAt(inputOffset) == '=') {
            inputOffset += 1
            nextToken = Token.NE
            return
          }
          throw new XPathException("'!' without '='")

        case '*' => 
          if (inputOffset < inputLength && input.charAt(inputOffset) == ':') {
            inputOffset += 1
            nextToken = Token.SUFFIX
            if (inputOffset < inputLength) {
              val ahead = input.charAt(inputOffset)
              if (" \r\t\n(".indexOf(ahead) >= 0) {
                throw new XPathException("Whitespace and comments are not allowed after '*:'")
              }
            }
            return
          }
          nextToken = Token.STAR
          return

        case ',' => 
          nextToken = Token.COMMA
          return

        case '$' => 
          nextToken = Token.DOLLAR
          return

        case '|' => 
          nextToken = Token.UNION
          return

        case '#' => 
          nextToken = Token.HASH
          return

        case '<' => 
          if (inputOffset < inputLength && input.charAt(inputOffset) == '=') {
            inputOffset += 1
            nextToken = Token.LE
            return
          }
          if (inputOffset < inputLength && input.charAt(inputOffset) == '<') {
            inputOffset += 1
            nextToken = Token.PRECEDES
            return
          }
          nextToken = Token.LT
          return

        case '>' => 
          if (inputOffset < inputLength && input.charAt(inputOffset) == '=') {
            inputOffset += 1
            nextToken = Token.GE
            return
          }
          if (inputOffset < inputLength && input.charAt(inputOffset) == '>') {
            inputOffset += 1
            nextToken = Token.FOLLOWS
            return
          }
          nextToken = Token.GT
          return

        case '.' => 
          if (inputOffset < inputLength && input.charAt(inputOffset) == '.') {
            inputOffset += 1
            nextToken = Token.DOTDOT
            return
          }
          if (inputOffset == inputLength || input.charAt(inputOffset) < '0' || 
            input.charAt(inputOffset) > '9') {
            nextToken = Token.DOT
            return
          }

        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => 
          var allowE = true
          var allowSign = false
          var allowDot = true
          var endOfNum = false
          numloop: while (!endOfNum) c match {
            case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => allowSign = false
            case '.' => if (allowDot) {
              allowDot = false
              allowSign = false
            } else {
              inputOffset -= 1
              //break
            }
            case 'E' | 'e' => if (allowE) {
              allowSign = true
              allowE = false
            } else {
              inputOffset -= 1
              //break
            }
            case '+' | '-' => if (allowSign) {
              allowSign = false
            } else {
              inputOffset -= 1
              //break
            }
            case _ => 
              if (('a' <= c && c <= 'z') || c > 127) {
                throw new XPathException("Separator needed after numeric literal")
              }
              inputOffset -= 1

          }
          nextTokenValue = input.substring(nextTokenStartOffset, inputOffset)
          nextToken = Token.NUMBER
          return

        case '"' | '\'' => 
          nextTokenValue = ""
          while (true) {
            inputOffset = input.indexOf(c, inputOffset)
            if (inputOffset < 0) {
              inputOffset = nextTokenStartOffset + 1
              throw new XPathException("Unmatched quote in expression")
            }
            nextTokenValue += input.substring(nextTokenStartOffset + 1, inputOffset += 1)
            if (inputOffset < inputLength) {
              val n = input.charAt(inputOffset)
              if (n == c) {
                nextTokenValue += c
                nextTokenStartOffset = inputOffset
                inputOffset += 1
              } else {
                //break
              }
            } else {
              //break
            }
          }
          nextToken = Token.STRING_LITERAL
          return

        case '\n' | ' ' | '\t' | '\r' => nextTokenStartOffset = inputOffset
        case _ => if (c < 0x80 && !Character.isLetter(c)) {
          throw new XPathException("Invalid character '" + c + "' in expression")
        }
        case '_' => 
          loop: while (inputOffset < inputLength) {
            c = input.charAt(inputOffset)
            c match {
              case ':' => if (inputOffset + 1 < inputLength) {
                val nc = input.charAt(inputOffset + 1)
                if (nc == ':') {
                  nextTokenValue = input.substring(nextTokenStartOffset, inputOffset)
                  nextToken = Token.AXIS
                  inputOffset += 2
                  return
                } else if (nc == '*') {
                  nextTokenValue = input.substring(nextTokenStartOffset, inputOffset)
                  nextToken = Token.PREFIX
                  inputOffset += 2
                  return
                } else if (nc == '=') {
                  nextTokenValue = input.substring(nextTokenStartOffset, inputOffset)
                  nextToken = Token.NAME
                  return
                }
              }
              case '.' | '-' | '_' => //break
              case _ => if (c < 0x80 && !Character.isLetterOrDigit(c)) //break
            }
            inputOffset += 1
          }
          nextTokenValue = input.substring(nextTokenStartOffset, inputOffset)
          nextToken = Token.NAME
          return

      }
    }
  }

  /**
   * Test whether the previous token is an operator
   * @param precedingToken the token to be tested
   * @return true if the previous token is an operator token
   */
  private def followsOperator(precedingToken: Int): Boolean = precedingToken <= Token.LAST_OPERATOR

  /**
   * Get the most recently read text (for use in an error message)
   * @param offset the offset of the offending token, if known, or -1 to use the current offset
   * @return a chunk of text leading up to the error
   */
  def recentText(offset: Int): String = {
    if (offset == -1) {
      if (inputOffset > inputLength) {
        inputOffset = inputLength
      }
      if (inputOffset < 34) {
        input.substring(0, inputOffset)
      } else {
        Whitespace.collapseWhitespace("..." + input.substring(inputOffset - 30, inputOffset))
          .toString
      }
    } else {
      var end = offset + 30
      if (end > inputLength) {
        end = inputLength
      }
      Whitespace.collapseWhitespace((if (offset > 0) "..." else "") + input.substring(offset, end))
        .toString
    }
  }
}
