// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr

import org.orbeon.darius.xpath.`type`.ItemType
import org.orbeon.darius.xpath.expr.RoleLocator._
import org.orbeon.darius.xpath.om.StructuredQName

object RoleLocator {

  val FUNCTION = 0
  val BINARY_EXPR = 1
  val TYPE_OP = 2
  val VARIABLE = 3
  val INSTRUCTION = 4
  val FUNCTION_RESULT = 5
  val ORDER_BY = 6
  val TEMPLATE_RESULT = 7
  val PARAM = 8
  val UNARY_EXPR = 9
  val UPDATING_EXPR = 10
  val GROUPING_KEY = 11
  val EVALUATE_RESULT = 12

  /**
   * Get the ordinal representation of a number (used to identify which argument of a function
   * is in error)
   * @param n the cardinal number
   * @return the ordinal representation
   */
  def ordinal(n: Int): String = n match {
    case 1 ⇒ "first"
    case 2 ⇒ "second"
    case 3 ⇒ "third"
    case _ ⇒ n + "th"
  }
}

/**
 * A RoleLocator identifies the role in which an expression is used, for example as
 * the third argument of the concat() function. This information is stored in an
 * ItemChecker or CardinalityChecker so that good diagnostics can be
 * achieved when run-time type errors are detected.
 */
class RoleLocator(var kind: Int, var operation: AnyRef, var operand: Int) {

  private var _errorCode: String = "XPTY0004"

  if (!(operation.isInstanceOf[String] || operation.isInstanceOf[StructuredQName])) {
    throw new IllegalArgumentException("operation")
  }

  /**
   * Set the error code to be produced if a type error is detected
   * @param code The error code
   */
  def setErrorCode(code: String): Unit = {
    if (code != null) {
      _errorCode = code
    }
  }

  def getErrorCode = _errorCode

  /**
   * Construct and return the error message indicating a type error
   * @return the constructed error message
   */
  def getMessage: String = {
    var name: String = null
    name = operation match {
      case s: String              ⇒ s
      case qName: StructuredQName ⇒ qName.getDisplayName
    }
    kind match {
      case FUNCTION ⇒ ordinal(operand + 1) + " argument of " +
        (if (name.length == 0) "anonymous function" else name + "()")
      case BINARY_EXPR ⇒ ordinal(operand + 1) + " operand of '" + name + '\''
      case UNARY_EXPR ⇒ "operand of '-'"
      case TYPE_OP ⇒ "value in '" + name + "' expression"
      case VARIABLE ⇒ "value of variable $" + name
      case INSTRUCTION ⇒
        var slash = name.indexOf('/')
        var attributeName = ""
        if (slash >= 0) {
          attributeName = name.substring(slash + 1)
          name = name.substring(0, slash)
        }
        '@' + attributeName + " attribute of " + name

      case FUNCTION_RESULT ⇒ if (name.length == 0) {
        "result of anonymous function"
      } else {
        "result of function " + name + "()"
      }
      case TEMPLATE_RESULT ⇒ "result of template " + name
      case ORDER_BY ⇒ ordinal(operand + 1) + " sort key"
      case PARAM ⇒ "value of parameter $" + name
      case UPDATING_EXPR ⇒ "value of " + ordinal(operand + 1) + " operand of " +
        name + 
        " expression"
      case GROUPING_KEY ⇒ "value of the grouping key"
      case EVALUATE_RESULT ⇒ "result of the expression {" + name + "} evaluated by xsl:evaluate"
      case _ ⇒ ""
    }
  }

  /**
   * Construct the part of the message giving the required item type
   *
   * @param requiredItemType the item type required by the context of a particular expression
   * @return a message of the form "Required item type of X is Y"
   */
  def composeRequiredMessage(requiredItemType: ItemType): String = {
    "Required item type of " + getMessage + " is " + requiredItemType.toString
  }

  /**
   * Construct a full error message
   *
   * @param requiredItemType the item type required by the context of a particular expression
   * @param suppliedItemType the item type inferred by static analysis of an expression
   * @return a message of the form "Required item type of A is R; supplied value has item type S"
   */
  def composeErrorMessage(requiredItemType: ItemType, suppliedItemType: ItemType): String = {
    composeRequiredMessage(requiredItemType) + "; supplied value has item type " + 
      suppliedItemType.toString
  }
}
