// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.style

import org.orbeon.darius.xpath.expr.Expression
import org.orbeon.darius.xpath.expr.RoleLocator
import org.orbeon.darius.xpath.expr.StringLiteral
import org.orbeon.darius.xpath.expr.TypeChecker
import org.orbeon.darius.xpath.expr.instruct.Executable
import org.orbeon.darius.xpath.expr.instruct.NumberInstruction
import org.orbeon.darius.xpath.expr.instruct.ValueOf
import org.orbeon.darius.xpath.expr.number.NumberFormatter
import org.orbeon.darius.xpath.expr.number.Numberer_en
import org.orbeon.darius.xpath.lib.Numberer
import org.orbeon.darius.xpath.pattern.NodeKindTest
import org.orbeon.darius.xpath.pattern.Pattern
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.`type`.ItemType
import org.orbeon.darius.xpath.value.SequenceType
import org.orbeon.darius.xpath.value.StringValue
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

  def prepareAttributes(): Unit = {
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

  def validate(decl: Declaration): Unit = {
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
        select = TypeChecker.staticTypeCheck(select, SequenceType.SINGLE_NODE, backwardsCompatible = false, role)
      } catch {
        case err: XPathException ⇒ compileError(err)
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
