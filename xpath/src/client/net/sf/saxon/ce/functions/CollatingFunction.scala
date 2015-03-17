// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import java.net.{URI, URISyntaxException}

import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.expr.sort.{CodepointCollator, GenericAtomicComparer}
import client.net.sf.saxon.ce.lib.StringCollator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.{AtomicValue, StringValue}

abstract class CollatingFunction extends SystemFunction {

  protected var stringCollator: StringCollator = null

  private var staticContext: StaticContext = null

  override def checkArguments(visitor: ExpressionVisitor) {
    if (staticContext == null) {
      staticContext = visitor.getStaticContext
    }
    if (stringCollator == null) {
      preEvaluateCollation(staticContext)
    }
    super.checkArguments(visitor)
  }

  /**
   * Pre-evaluate the collation argument if its value is known statically
   * @param env the static XPath evaluation context
   */
  private def preEvaluateCollation(env: StaticContext) {
    if (getNumberOfArguments == getDetails.maxArguments) {
      val collationExp = argument(getNumberOfArguments - 1)
      val collationVal = (if (collationExp.isInstanceOf[Literal]) collationExp.asInstanceOf[Literal].getValue else null)
      if (collationVal.isInstanceOf[AtomicValue]) {
        var collationName = collationVal.asInstanceOf[AtomicValue].getStringValue
        collationName = resolveCollationURI(collationName)
        stringCollator = env.getConfiguration.getNamedCollation(collationName)
      } else {
      }
    } else {
      val uri = env.getDefaultCollationName
      stringCollator = env.getConfiguration.getNamedCollation(uri)
    }
  }

  /**
   * Get a GenericAtomicComparer that can be used to compare values. This method is called
   * at run time by subclasses to evaluate the parameter containing the collation name.
   * <p/>
   * <p>The difference between this method and [[getCollator]] is that a
   * GenericAtomicComparer is capable of comparing values of any atomic type, not only
   * strings. It is therefore called by functions such as compare, deep-equal, index-of, and
   * min() and max() where the operands may include a mixture of strings and other types.</p>
   *
   * @param arg     the position of the argument (starting at 0) containing the collation name.
   *                If this argument was not supplied, the default collation is used
   * @param context The dynamic evaluation context.
   * @return a GenericAtomicComparer that can be used to compare atomic values.
   */
  protected def getAtomicComparer(arg: Int, context: XPathContext): GenericAtomicComparer = {
    new GenericAtomicComparer(getCollator(arg, context), context.getImplicitTimezone)
  }

  /**
   * Get a collator suitable for comparing strings. Returns the collator specified in the
   * given function argument if present, otherwise returns the default collator. This method is
   * called by subclasses at run time. It is used (in contrast to [[getAtomicComparer]])
   * when it is known that the values to be compared are always strings.
   *
   * @param arg     The argument position (counting from zero) that holds the collation
   *                URI if present
   * @param context The dynamic context
   * @return a StringCollator
   */
  protected def getCollator(arg: Int, context: XPathContext): StringCollator = {
    if (stringCollator != null) {
      stringCollator
    } else {
      val numargs = argument.length
      if (numargs > arg) {
        val av = argument(arg).evaluateItem(context).asInstanceOf[AtomicValue]
        val collationValue = av.asInstanceOf[StringValue]
        var collationName = collationValue.getStringValue
        collationName = resolveCollationURI(collationName)
        context.getConfiguration.getNamedCollation(collationName)
      } else {
        CodepointCollator.getInstance
      }
    }
  }

  private def resolveCollationURI(collationName: String): String = {
    var collationURI: URI = null
    try {
      collationURI = new URI(collationName)//ORBEON true
      if (!collationURI.isAbsolute) {
        val expressionBaseURI = staticContext.getBaseURI
        if (expressionBaseURI == null) {
          val err = new XPathException("Cannot resolve relative collation URI '" + collationName + 
            "': unknown or invalid base URI")
          err.setErrorCode("FOCH0002")
          err.setLocator(this.getSourceLocator)
          throw err
        }
        collationURI = new URI(expressionBaseURI).resolve(collationURI.toString)
        collationURI.toString
      } else {
        collationName
      }
    } catch {
      case e: URISyntaxException => {
        val err = new XPathException("Collation name '" + collationName + "' is not a valid URI")
        err.setErrorCode("FOCH0002")
        err.setLocator(this.getSourceLocator)
        throw err
      }
    }
  }

  protected def doesNotSupportSubstringMatching(context: XPathContext) {
    dynamicError("The collation requested for " + getDisplayName + " does not support substring matching", 
      "FOCH0004")
  }
}
