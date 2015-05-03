// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr.instruct

import org.orbeon.darius.xpath.expr._
import org.orbeon.darius.xpath.expr.number.NumberFormatter
import org.orbeon.darius.xpath.expr.number.Numberer_en
import org.orbeon.darius.xpath.functions.NumberFn
import org.orbeon.darius.xpath.lib.Numberer
import org.orbeon.darius.xpath.om.Item
import org.orbeon.darius.xpath.om.NodeInfo
import org.orbeon.darius.xpath.om.SequenceIterator
import org.orbeon.darius.xpath.orbeon.Configuration
import org.orbeon.darius.xpath.pattern.Pattern
import org.orbeon.darius.xpath.pattern.PatternSponsor
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.tree.util.Navigator
import org.orbeon.darius.xpath.`type`._
import org.orbeon.darius.xpath.value._
import org.orbeon.darius.xpath.value.StringValue
import java.util.ArrayList
import java.util.Collections
import java.util.Iterator
import java.util.List
import NumberInstruction._
//remove if not needed
import scala.collection.JavaConversions._

object NumberInstruction {

  private val SINGLE = 0

  private val MULTI = 1

  private val ANY = 2

  private val SIMPLE = 3

  private val SELECT = 0

  private val VALUE = 1

  private val FORMAT = 2

  private val GROUP_SIZE = 3

  private val GROUP_SEPARATOR = 4

  private val LETTER_VALUE = 5

  private val ORDINAL = 6

  private val LANG = 7

  private val ARGS = 8
}

/**
 * An xsl:number element in the stylesheet. Although this is an XSLT instruction, it is compiled
 * into an expression, evaluated using xsl:value-of to create the resulting text node.<br>
 */
class NumberInstruction(config: Configuration, 
    select: Expression, 
    var level: Int, 
    var count: Pattern, 
    var from: Pattern, 
    value: Expression, 
    format: Expression, 
    groupSize: Expression, 
    groupSeparator: Expression, 
    letterValue: Expression, 
    ordinal: Expression, 
    lang: Expression, 
    var formatter: NumberFormatter, 
    var numberer: Numberer, 
    var hasVariablesInPatterns: Boolean, 
    var backwardsCompatible: Boolean) extends Expression {

  private var arguments: Array[Expression] = new Array[Expression](8)

  arguments(SELECT) = select

  arguments(VALUE) = value

  arguments(FORMAT) = format

  arguments(GROUP_SIZE) = groupSize

  arguments(GROUP_SEPARATOR) = groupSeparator

  arguments(LETTER_VALUE) = letterValue

  arguments(ORDINAL) = ordinal

  arguments(LANG) = lang

  val th = TypeHierarchy.getInstance

  if (arguments(VALUE) != null && 
    !arguments(VALUE).getItemType.isInstanceOf[AtomicType]) {
    arguments(VALUE) = new Atomizer(arguments(VALUE))
  }

  val kids = iterateSubExpressions()

  while (kids.hasNext) {
    val child = kids.next()
    adoptChildExpression(child)
  }

  def simplify(visitor: ExpressionVisitor): Expression = {
    for (i ← 0 until ARGS) {
      arguments(i) = visitor.simplify(arguments(i))
    }
    if (count != null) {
      count = count.simplify(visitor)
    }
    if (from != null) {
      from = from.simplify(visitor)
    }
    this
  }

  /**
   * Perform static analysis of an expression and its subexpressions.
   *
   * <p>This checks statically that the operands of the expression have
   * the correct type; if necessary it generates code to do run-time type checking or type
   * conversion. A static type error is reported only if execution cannot possibly succeed, that
   * is, if a run-time type error is inevitable. The call may return a modified form of the expression.</p>
   *
   * <p>This method is called after all references to functions and variables have been resolved
   * to the declaration of the function or variable. However, the types of such functions and
   * variables will only be accurately known if they have been explicitly declared.</p>
   *
   * @param visitor an expression visitor
   * @param contextItemType the static type of "." at the point where this expression is invoked.
   * The parameter is set to null if it is known statically that the context item will be undefined.
   * If the type of the context item is not known statically, the argument is set to
   * [[org.orbeon.darius.xpath.type.Type#ITEM_TYPE]]
   * @throws XPathException if an error is discovered during this phase
   *     (typically a type error)
   * @return the original expression, rewritten to perform necessary
   *     run-time type checks, and to perform other type-related
   *     optimizations
   */
  def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    if (arguments(SELECT) == null && arguments(VALUE) == null) {
      var err: XPathException = null
      if (contextItemType == null) {
        err = new XPathException("xsl:number requires a select attribute, a value attribute, or a context item")
      } else if (contextItemType.isInstanceOf[AtomicType]) {
        err = new XPathException("xsl:number requires the context item to be a node, but it is an atomic value")
      }
      if (err != null) {
        err.setIsTypeError(true)
        err.setErrorCode("XTTE0990")
        err.setLocator(getSourceLocator)
        throw err
      }
    }
    for (i ← 0 until ARGS) {
      arguments(i) = visitor.typeCheck(arguments(i), contextItemType)
    }
    if (count != null) {
      visitor.typeCheck(new PatternSponsor(count), contextItemType)
    }
    if (from != null) {
      visitor.typeCheck(new PatternSponsor(from), contextItemType)
    }
    this
  }

  /**
   * Perform optimisation of an expression and its subexpressions.
   * <p/>
   * <p>This method is called after all references to functions and variables have been resolved
   * to the declaration of the function or variable, and after all type checking has been done.</p>
   *
   * @param visitor an expression visitor
   * @param contextItemType the static type of "." at the point where this expression is invoked.
   *                        The parameter is set to null if it is known statically that the context item will be undefined.
   *                        If the type of the context item is not known statically, the argument is set to
   *                        [[org.orbeon.darius.xpath.type.Type#ITEM_TYPE]]
   * @return the original expression, rewritten if appropriate to optimize execution
   * @throws XPathException if an error is discovered during this phase
   *                                        (typically a type error)
   */
  def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    for (i ← 0 until ARGS) {
      arguments(i) = visitor.optimize(arguments(i), contextItemType)
    }
    this
  }

  /**
   * Get the immediate sub-expressions of this expression. Default implementation
   * returns a zero-length array, appropriate for an expression that has no
   * sub-expressions.
   * @return an iterator containing the sub-expressions of this expression
   */
  def iterateSubExpressions(): Iterator[Expression] = {
    val sub = new ArrayList[Expression](9)
    for (i ← 0 until ARGS if arguments(i) != null) {
      sub.add(arguments(i))
    }
    if (count != null) {
      sub.add(new PatternSponsor(count))
    }
    if (from != null) {
      sub.add(new PatternSponsor(from))
    }
    sub.iterator()
  }

  /**
   * Determine the intrinsic dependencies of an expression, that is, those which are not derived
   * from the dependencies of its subexpressions. For example, position() has an intrinsic dependency
   * on the context position, while (position()+1) does not. The default implementation
   * of the method returns 0, indicating "no dependencies".
   *
   * @return a set of bit-significant flags identifying the "intrinsic"
   *         dependencies. The flags are documented in class client.net.sf.saxon.ce.value.StaticProperty
   */
  def getIntrinsicDependencies: Int = {
    if ((arguments(SELECT) == null && arguments(VALUE) == null)) StaticProperty.DEPENDS_ON_CONTEXT_ITEM else 0
  }

  def getItemType: ItemType = AtomicType.STRING

  def computeCardinality(): Int = StaticProperty.EXACTLY_ONE

  /**
   * Offer promotion for this subexpression. The offer will be accepted if the subexpression
   * is not dependent on the factors (e.g. the context item) identified in the PromotionOffer.
   * By default the offer is not accepted - this is appropriate in the case of simple expressions
   * such as constant values and variable references where promotion would give no performance
   * advantage. This method is always called at compile time.
   *
   * @param offer details of the offer, for example the offer to move
   *              expressions that don't depend on the context to an outer level in
   *              the containing expression
   * @param parent
   * @return if the offer is not accepted, return this expression unchanged.
   *         Otherwise return the result of rewriting the expression to promote
   *         this subexpression
   * @throws org.orbeon.darius.xpath.trans.XPathException
   *          if any error is detected
   */
  def promote(offer: PromotionOffer, parent: Expression): Expression = {
    val exp = offer.accept(parent, this)
    if (exp != null) {
      exp
    } else {
      for (i ← 0 until ARGS if arguments(i) != null) {
        arguments(i) = doPromotion(arguments(i), offer)
      }
      if (count != null) {
        count.promote(offer, this)
      }
      if (from != null) {
        from.promote(offer, this)
      }
      this
    }
  }

  def evaluateItem(context: XPathContext): Item = {
    var value = -1
    var vec: List[_] = null
    if (arguments(VALUE) != null) {
      val iter = arguments(VALUE).iterate(context)
      vec = new ArrayList(4)
      while (true) {
        val `val` = iter.next().asInstanceOf[AtomicValue]
        if (`val` == null) {
          //break
        }
        if (backwardsCompatible && !vec.isEmpty) {
          //break
        }
        try {
          var num: NumericValue = null
          num = if (`val`.isInstanceOf[NumericValue]) `val`.asInstanceOf[NumericValue] else NumberFn.convert(`val`)
          if (num.isNaN) {
            throw new XPathException("NaN")
          }
          num = num.round()
          if (num.signum() >= 0) {
            val i = num.convert(AtomicType.INTEGER).asAtomic().asInstanceOf[NumericValue]
              .intValue()
            vec.add(i)
          } else {
            if (num.compareTo(IntegerValue.ZERO) < 0) {
              throw new XPathException("The numbers to be formatted must not be negative")
            }
            val i = num.convert(AtomicType.INTEGER).asAtomic().asInstanceOf[NumericValue]
              .intValue()
            vec.add(i)
          }
        } catch {
          case err: XPathException ⇒ if (backwardsCompatible) {
            vec.add("NaN")
          } else {
            vec.add(`val`.getStringValue)
            throw new XPathException("Cannot convert supplied value to an integer. " + err.getMessage, 
              "XTDE0980")
          }
        }
      }
      if (backwardsCompatible && vec.isEmpty) {
        vec.add("NaN")
      }
    } else {
      var source: NodeInfo = null
      if (arguments(SELECT) != null) {
        source = arguments(SELECT).evaluateItem(context).asInstanceOf[NodeInfo]
      } else {
        val item = context.getContextItem
        if (!item.isInstanceOf[NodeInfo]) {
          val err = new XPathException("context item for xsl:number must be a node", "XTTE0990")
          err.setIsTypeError(true)
          throw err
        }
        source = item.asInstanceOf[NodeInfo]
      }
      if (level == SIMPLE) {
        value = Navigator.getNumberSimple(source)
      } else if (level == SINGLE) {
        value = Navigator.getNumberSingle(source, count, from, context)
        if (value == 0) {
          vec = Collections.EMPTY_LIST
        }
      } else if (level == ANY) {
        value = Navigator.getNumberAny(this, source, count, from, context, hasVariablesInPatterns)
        if (value == 0) {
          vec = Collections.EMPTY_LIST
        }
      } else if (level == MULTI) {
        vec = Navigator.getNumberMulti(source, count, from, context)
      }
    }
    var gpsize = 0
    var gpseparator = ""
    var letterVal: String = null
    var ordinalVal: String = null
    if (arguments(GROUP_SIZE) != null) {
      val g = arguments(GROUP_SIZE).evaluateAsString(context).toString
      gpsize = Integer.parseInt(g)
    }
    if (arguments(GROUP_SEPARATOR) != null) {
      gpseparator = arguments(GROUP_SEPARATOR).evaluateAsString(context)
        .toString
    }
    if (arguments(ORDINAL) != null) {
      ordinalVal = arguments(ORDINAL).evaluateAsString(context).toString
    }
    if (vec == null && arguments(FORMAT) == null && gpsize == 0 && 
      arguments(LANG) == null) {
      return new StringValue("" + value)
    }
    var numb = numberer
    if (numb == null) {
      val language = arguments(LANG).evaluateAsString(context).toString
      if (!StringValue.isValidLanguageCode(language)) {
        throw new XPathException("The lang attribute of xsl:number must be a valid language code", "XTDE0030")
      }
      numb = new Numberer_en()
    }
    if (arguments(LETTER_VALUE) == null) {
      letterVal = ""
    } else {
      letterVal = arguments(LETTER_VALUE).evaluateAsString(context).toString
      if (!("alphabetic" == letterVal || "traditional" == letterVal)) {
        throw new XPathException("letter-value must be \"traditional\" or \"alphabetic\"", "XTDE0030")
      }
    }
    if (vec == null) {
      vec = new ArrayList(1)
      vec.add(value)
    }
    var nf: NumberFormatter = null
    if (formatter == null) {
      nf = new NumberFormatter()
      nf.prepare(arguments(FORMAT).evaluateAsString(context).toString)
    } else {
      nf = formatter
    }
    val s = nf.format(vec, gpsize, gpseparator, letterVal, ordinalVal, numb)
    new StringValue(s)
  }
}
