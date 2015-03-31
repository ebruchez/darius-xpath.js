// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.`type`.{AnyItemType, AtomicType, ItemType}
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.SequenceType

import scala.beans.BeanProperty

object SystemFunction {

  /**
   * Make a system function call (one in the standard function namespace).
   * @param name The local name of the function.
   * @param arguments the arguments to the function call
   * @return a FunctionCall that implements this function, if it
   * exists, or null if the function is unknown.
   */
  def makeSystemFunction(name: String, arguments: Array[Expression]): FunctionCall = {
    val entry = StandardFunction.getFunction(name, arguments.length)
    if (entry == null) {
      return null
    }
    val f = entry.skeleton.newInstance()
    f.setDetails(entry)
    f.setFunctionName(new StructuredQName("", NamespaceConstant.FN, name))
    f.setArguments(arguments)
    f
  }
}

/**
 * Abstract superclass for system-defined and user-defined functions
 */
abstract class SystemFunction extends FunctionCall {

  @BeanProperty
  var details: StandardFunction.Entry = _

  protected var operation: Int = _

  /**
   * Method called during static type checking
   */
  def checkArguments(visitor: ExpressionVisitor): Unit = {
    checkArgumentCount(details.minArguments, details.maxArguments, visitor)
    for (i <- 0 until argument.length) {
      checkArgument(i, visitor)
    }
  }

  /**
   * Perform static type checking on an argument to a function call, and add
   * type conversion logic where necessary.
   * @param arg argument number, zero-based
   * @param visitor an expression visitor
   * @throws XPathException if type checking fails
   */
  private def checkArgument(arg: Int, visitor: ExpressionVisitor): Unit = {
    val role = new RoleLocator(RoleLocator.FUNCTION, getFunctionName, arg)
    role.setErrorCode(getErrorCodeForTypeErrors)
    argument(arg) = TypeChecker.staticTypeCheck(argument(arg), getRequiredType(arg), visitor.getStaticContext.isInBackwardsCompatibleMode, 
      role)
  }

  /**
   * Copy an expression. This makes a deep copy.
   *
   * @return the copy of the original expression
   */
  def newInstance(): SystemFunction

  /**
   * Return the error code to be used for type errors. This is overridden for functions
   * such as exactly-one(), one-or-more(), ...
   * @return the error code to be used for type errors in the function call. Normally XPTY0004,
   * but different codes are used for functions such as exactly-one()
   */
  def getErrorCodeForTypeErrors(): String = "XPTY0004"

  /**
   * Get the required type of the nth argument
   * @param arg the number of the argument whose type is requested, zero-based
   * @return the required type of the argument as defined in the function signature
   */
  protected def getRequiredType(arg: Int): SequenceType = {
    if (details == null) {
      return SequenceType.ANY_SEQUENCE
    }
    details.argumentTypes(arg)
  }

  /**
   * Determine the item type of the value returned by the function
   */
  def getItemType(): ItemType = {
    val `type` = details.resultType.getPrimaryType
    if (details.sameItemTypeAsFirstArgument) {
      if (argument.length > 0) {
        argument(0).getItemType
      } else {
        AnyItemType.getInstance
      }
    } else {
      `type`
    }
  }

  /**
   * Determine the cardinality of the function.
   */
  def computeCardinality(): Int = details.resultType.getCardinality

  /**
   * Determine the special properties of this expression. The general rule
   * is that a system function call is non-creative if its return type is
   * atomic, or if all its arguments are non-creative. This is overridden
   * for the generate-id() function, which is considered creative if
   * its operand is creative (because the result depends on the
   * identity of the operand)
   */
  override def computeSpecialProperties(): Int = {
    val p = super.computeSpecialProperties()
    if (details == null) {
      return p
    }
    if (details.resultType.getPrimaryType.isInstanceOf[AtomicType]) {
      return p | StaticProperty.NON_CREATIVE
    }
    for (arg <- argument if (arg.getSpecialProperties & StaticProperty.NON_CREATIVE) ==
      0) {
      return p
    }
    p | StaticProperty.NON_CREATIVE
  }

  /**
   * Set "." as the default value for the first and only argument. Called from subclasses.
   */
  protected def useContextItemAsDefault(): Unit = {
    if (argument.length == 0) {
      argument = new Array[Expression](1)
      argument(0) = new ContextItemExpression()
      ExpressionTool.copyLocationInfo(this, argument(0))
      resetLocalStaticProperties()
    }
  }

  /**
   * Add an implicit argument referring to the context document. Called by functions such as
   * id() and key() that take the context document as an implicit argument
   * @param pos the position of the argument whose default value is ".", zero-based
   * @param augmentedName the name to be used for the function call with its extra argument.
   * There are some cases where user function calls cannot supply the argument directly (notably
   * unparsed-entity-uri() and unparsed-entity-public-id()) and in these cases a synthesized
   * function name is used for the new function call.
   * @throws XPathException if an error occurs
   */
  protected def addContextDocumentArgument(pos: Int, augmentedName: String): Unit = {
    if (argument.length > pos) {
      return
    }
    if (argument.length != pos) {
      throw new XPathException("Too few arguments in call to " + augmentedName + "() function")
    }
    val newArgs = new Array[Expression](pos + 1)
    System.arraycopy(argument, 0, newArgs, 0, argument.length)
    val rootExpression = new RootExpression()
    ExpressionTool.copyLocationInfo(this, rootExpression)
    newArgs(pos) = rootExpression
    argument = newArgs
    setDetails(StandardFunction.getFunction(augmentedName, newArgs.length))
  }
}
