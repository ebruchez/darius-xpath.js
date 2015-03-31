// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.functions.Concat
import client.net.sf.saxon.ce.functions.SystemFunction
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.util.SourceLocator
import client.net.sf.saxon.ce.`type`.AtomicType
import client.net.sf.saxon.ce.`type`.TypeHierarchy
import client.net.sf.saxon.ce.value.Cardinality
import client.net.sf.saxon.ce.value.StringValue
import java.util.ArrayList
import java.util.List
//remove if not needed
import scala.collection.JavaConversions._

object AttributeValueTemplate {

  /**
   * Static factory method to create an AVT from an XSLT string representation.
   */
  def make(avt: String, sourceLocator: SourceLocator, env: StaticContext): Expression = {
    val components = new ArrayList(5)
    var i0: Int = 0
    var i1: Int = 0
    var i8: Int = 0
    var i9: Int = 0
    val len = avt.length
    var last = 0
    val visitor = ExpressionVisitor.make(env, null)
    while (last < len) {
      i0 = avt.indexOf("{", last)
      i1 = avt.indexOf("{{", last)
      i8 = avt.indexOf("}", last)
      i9 = avt.indexOf("}}", last)
      if ((i0 < 0 || len < i0) && (i8 < 0 || len < i8)) {
        addStringComponent(components, avt, last, len)
        //break
      } else if (i8 >= 0 && (i0 < 0 || i8 < i0)) {
        if (i8 != i9) {
          val err = new XPathException("Closing curly brace in attribute value template \"" + 
            avt.substring(0, len) + 
            "\" must be doubled")
          err.setErrorCode("XTSE0370")
          err.setIsStaticError(true)
          throw err
        }
        addStringComponent(components, avt, last, i8 + 1)
        last = i8 + 2
      } else if (i1 >= 0 && i1 == i0) {
        addStringComponent(components, avt, last, i1 + 1)
        last = i1 + 2
      } else if (i0 >= 0) {
        if (i0 > last) {
          addStringComponent(components, avt, last, i0)
        }
        var exp: Expression = null
        val parser = new ExpressionParser()
        parser.setDefaultContainer(env.asInstanceOf[ExpressionContext].getStyleElement)
        parser.setLanguage(ExpressionParser.XPATH)
        exp = parser.parse(avt, i0 + 1, Token.RCURLY, env)
        exp = visitor.simplify(exp)
        last = parser.getTokenizer.currentTokenStartOffset + 1
        if (env.isInBackwardsCompatibleMode) {
          components.add(makeFirstItem(exp))
        } else {
          components.add(visitor.simplify(XSLLeafNodeConstructor.makeSimpleContentConstructor(exp, new StringLiteral(StringValue.SINGLE_SPACE))))
        }
      } else {
        throw new IllegalStateException("Internal error parsing AVT")
      }
    }
    if (components.size == 0) {
      return new StringLiteral(StringValue.EMPTY_STRING)
    }
    if (components.size == 1) {
      return visitor.simplify(components.get(0).asInstanceOf[Expression])
    }
    val args = Array.ofDim[Expression](components.size)
    components.toArray(args)
    val fn = SystemFunction.makeSystemFunction("concat", args).asInstanceOf[Concat]
    fn.setSourceLocator(sourceLocator)
    visitor.simplify(fn)
  }

  private def addStringComponent(components: List[_], 
      avt: String, 
      start: Int, 
      end: Int): Unit = {
    if (start < end) {
      components.add(new StringLiteral(avt.substring(start, end)))
    }
  }

  /**
   * Make an expression that extracts the first item of a sequence, after atomization
   */
  def makeFirstItem(exp: Expression): Expression = {
    val th = TypeHierarchy.getInstance
    if (!(exp.getItemType.isInstanceOf[AtomicType])) {
      exp = new Atomizer(exp)
    }
    if (Cardinality.allowsMany(exp.getCardinality)) {
      exp = new FirstItemExpression(exp)
    }
    if (!th.isSubType(exp.getItemType, AtomicType.STRING)) {
      exp = new AtomicSequenceConverter(exp, AtomicType.STRING)
    }
    exp
  }
}
