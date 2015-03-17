package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.RoleLocator
import client.net.sf.saxon.ce.expr.StringLiteral
import client.net.sf.saxon.ce.expr.TypeChecker
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.expr.instruct.NumberInstruction
import client.net.sf.saxon.ce.expr.instruct.ValueOf
import client.net.sf.saxon.ce.expr.number.NumberFormatter
import client.net.sf.saxon.ce.expr.number.Numberer_en
import client.net.sf.saxon.ce.lib.Numberer
import client.net.sf.saxon.ce.pattern.NodeKindTest
import client.net.sf.saxon.ce.pattern.Pattern
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.value.SequenceType
import client.net.sf.saxon.ce.value.StringValue
import XSLNumber._
//remove if not needed
import scala.collection.JavaConversions._

object XSLNumber {

  private val SINGLE = 0

  private val MULTI = 1

  private val ANY = 2

  private val SIMPLE = 3
}

/**
 * An xsl:number element in the stylesheet. <br>
 */
class XSLNumber extends StyleElement {

  private var level: Int = _

  private var count: Pattern = null

  private var from: Pattern = null

  private var select: Expression = null

  private var value: Expression = null

  private var format: Expression = null

  private var groupSize: Expression = null

  private var groupSeparator: Expression = null

  private var letterValue: Expression = null

  private var lang: Expression = null

  private var ordinal: Expression = null

  private var formatter: NumberFormatter = null

  private var numberer: Numberer = null

  private var hasVariablesInPatterns: Boolean = false

  /**
   * Determine whether this node is an instruction.
   * @return true - it is an instruction
   */
  def isInstruction(): Boolean = true

  /**
   * Determine the type of item returned by this instruction (only relevant if
   * it is an instruction).
   * @return the item type returned
   */
  protected def getReturnedItemType(): ItemType = NodeKindTest.TEXT

  def prepareAttributes() {
    var levelAtt: String = null
    select = checkAttribute("select", "e").asInstanceOf[Expression]
    value = checkAttribute("value", "e").asInstanceOf[Expression]
    count = checkAttribute("count", "p").asInstanceOf[Pattern]
    from = checkAttribute("from", "p").asInstanceOf[Pattern]
    levelAtt = checkAttribute("level", "w").asInstanceOf[String]
    format = checkAttribute("format", "a").asInstanceOf[Expression]
    lang = checkAttribute("lang", "a").asInstanceOf[Expression]
    letterValue = checkAttribute("letter-value", "a").asInstanceOf[Expression]
    groupSize = checkAttribute("grouping-size", "a").asInstanceOf[Expression]
    groupSeparator = checkAttribute("grouping-separator", "a").asInstanceOf[Expression]
    ordinal = checkAttribute("ordinal", "a").asInstanceOf[Expression]
    checkForUnknownAttributes()
    if (value != null) {
      if (select != null || count != null || from != null || levelAtt != null) {
        compileError("If the value attribute is present then select, count, from, and level must be absent", 
          "XTSE0975")
      }
    }
    if ((count != null && count.toString.indexOf('$') >= 0) || 
      (count != null && count.toString.indexOf('$') >= 0)) {
      hasVariablesInPatterns = true
    }
    if (levelAtt == null) {
      level = SINGLE
    } else if (levelAtt == "single") {
      level = SINGLE
    } else if (levelAtt == "multiple") {
      level = MULTI
    } else if (levelAtt == "any") {
      level = ANY
    } else {
      compileError("Invalid value for level attribute", "XTSE0020")
    }
    if (level == SINGLE && from == null && count == null) {
      level = SIMPLE
    }
    if (format != null) {
      if (format.isInstanceOf[StringLiteral]) {
        formatter = new NumberFormatter()
        formatter.prepare(format.asInstanceOf[StringLiteral].getStringValue)
      }
    } else {
      formatter = new NumberFormatter()
      formatter.prepare("1")
    }
    if ((groupSize == null) != (groupSeparator == null)) {
      groupSize = null
      groupSeparator = null
    }
    if (lang == null) {
      numberer = new Numberer_en()
    } else {
      if (lang.isInstanceOf[StringLiteral]) {
        val language = lang.asInstanceOf[StringLiteral].getStringValue
        if (language.length != 0) {
          if (!StringValue.isValidLanguageCode(language)) {
            compileError("The lang attribute must be a valid language code", "XTDE0030")
            lang = new StringLiteral(StringValue.EMPTY_STRING)
          }
        }
        numberer = new Numberer_en()
      }
    }
  }

  def validate(decl: Declaration) {
    checkEmpty()
    select = typeCheck(select)
    value = typeCheck(value)
    format = typeCheck(format)
    groupSize = typeCheck(groupSize)
    groupSeparator = typeCheck(groupSeparator)
    letterValue = typeCheck(letterValue)
    ordinal = typeCheck(ordinal)
    lang = typeCheck(lang)
    from = typeCheck("from", from)
    count = typeCheck("count", count)
    if (select != null) {
      try {
        val role = new RoleLocator(RoleLocator.INSTRUCTION, "xsl:number/select", 0)
        role.setErrorCode("XTTE1000")
        select = TypeChecker.staticTypeCheck(select, SequenceType.SINGLE_NODE, false, role)
      } catch {
        case err: XPathException => compileError(err)
      }
    }
  }

  def compile(exec: Executable, decl: Declaration): Expression = {
    val expr = new NumberInstruction(exec.getConfiguration, select, level, count, from, value, format, 
      groupSize, groupSeparator, letterValue, ordinal, lang, formatter, numberer, hasVariablesInPatterns, 
      xPath10ModeIsEnabled())
    expr.setSourceLocator(this)
    val inst = new ValueOf(expr, false)
    inst.setSourceLocator(this)
    inst.setIsNumberingInstruction()
    inst
  }
}
