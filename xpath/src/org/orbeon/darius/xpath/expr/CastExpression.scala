// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr

import org.orbeon.darius.xpath.`type`._
import org.orbeon.darius.xpath.expr.CastExpression._
import org.orbeon.darius.xpath.functions.StringFn
import org.orbeon.darius.xpath.om.{Item, StructuredQName}
import org.orbeon.darius.xpath.orbeon.HashMap
import org.orbeon.darius.xpath.pattern.EmptySequenceTest
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.value._

import scala.beans.BeanProperty

object CastExpression {

  var castingTable: HashMap[AtomicType, Array[AtomicType]] = new HashMap[AtomicType, Array[AtomicType]](25)

  def addAllowedCasts(source: AtomicType, target: Array[AtomicType]): Unit = {
    castingTable.put(source, target)
  }

  val uat = AtomicType.UNTYPED_ATOMIC

  val str = AtomicType.STRING

  val flt = AtomicType.FLOAT

  val dbl = AtomicType.DOUBLE

  val dec = AtomicType.DECIMAL

  val ing = AtomicType.INTEGER

  val dur = AtomicType.DURATION

  val ymd = AtomicType.YEAR_MONTH_DURATION

  val dtd = AtomicType.DAY_TIME_DURATION

  val dtm = AtomicType.DATE_TIME

  val tim = AtomicType.TIME

  val dat = AtomicType.DATE

  val gym = AtomicType.G_YEAR_MONTH

  val gyr = AtomicType.G_YEAR

  val gmd = AtomicType.G_MONTH_DAY

  val gdy = AtomicType.G_DAY

  val gmo = AtomicType.G_MONTH

  val boo = AtomicType.BOOLEAN

  val b64 = AtomicType.BASE64_BINARY

  val hxb = AtomicType.HEX_BINARY

  val uri = AtomicType.ANY_URI

  val qnm = AtomicType.QNAME

  val t01 = Array(uat, str, flt, dbl, dec, ing, dur, ymd, dtd, dtm, tim, dat, gym, gyr, gmd, gdy, gmo, boo, b64, hxb, uri)

  addAllowedCasts(uat, t01)

  val t02 = Array(uat, str, flt, dbl, dec, ing, dur, ymd, dtd, dtm, tim, dat, gym, gyr, gmd, gdy, gmo, boo, b64, hxb, uri, qnm)

  addAllowedCasts(str, t02)

  val t03 = Array(uat, str, flt, dbl, dec, ing, boo)

  addAllowedCasts(flt, t03)

  addAllowedCasts(dbl, t03)

  addAllowedCasts(dec, t03)

  addAllowedCasts(ing, t03)

  val t04 = Array(uat, str, dur, ymd, dtd)

  addAllowedCasts(dur, t04)

  addAllowedCasts(ymd, t04)

  addAllowedCasts(dtd, t04)

  val t05 = Array(uat, str, dtm, tim, dat, gym, gyr, gmd, gdy, gmo)

  addAllowedCasts(dtm, t05)

  val t06 = Array(uat, str, tim)

  addAllowedCasts(tim, t06)

  val t07 = Array(uat, str, dtm, dat, gym, gyr, gmd, gdy, gmo)

  addAllowedCasts(dat, t07)

  val t08 = Array(uat, str, gym)

  addAllowedCasts(gym, t08)

  val t09 = Array(uat, str, gyr)

  addAllowedCasts(gyr, t09)

  val t10 = Array(uat, str, gmd)

  addAllowedCasts(gmd, t10)

  val t11 = Array(uat, str, gdy)

  addAllowedCasts(gdy, t11)

  val t12 = Array(uat, str, gmo)

  addAllowedCasts(gmo, t12)

  val t13 = Array(uat, str, flt, dbl, dec, ing, boo)

  addAllowedCasts(boo, t13)

  val t14 = Array(uat, str, b64, hxb)

  addAllowedCasts(b64, t14)

  addAllowedCasts(hxb, t14)

  val t15 = Array(uat, str, uri)

  addAllowedCasts(uri, t15)

  val t16 = Array(uat, str, qnm)

  addAllowedCasts(qnm, t16)

  /**
   * Determine whether casting from a source type to a target type is possible
   * @param _source a primitive type (one that has an entry in the casting table)
   * @param target another primitive type
   * @return true if the entry in the casting table is either "Y" (casting always succeeds)
   * or "M" (casting allowed but may fail for some values)
   */
  def isPossibleCast(_source: AtomicType, target: AtomicType): Boolean = {
    var source = _source
    if (source == AtomicType.ANY_ATOMIC) {
      return true
    }
    if (source == AtomicType.NUMERIC) {
      source = AtomicType.DOUBLE
    }
    val targets = castingTable.get(source)
    if (targets == null) {
      return false
    }
    for (t ← targets if t == target) {
      return true
    }
    false
  }

  /**
   * Evaluate the "pseudo-cast" of a string literal to a QName or NOTATION value. This can only happen
   * at compile time
   *
   * @param operand the value to be converted
   * @param env the static context
   * @return the QName or NOTATION value that results from casting the string to a QName.
   * This will either be a QNameValue or a derived AtomicValue derived from QName or NOTATION
   */
  def castStringToQName(operand: CharSequence, env: StaticContext): AtomicValue = {
    try {
      val arg = Whitespace.trimWhitespace(operand).toString
      val qn = StructuredQName.fromLexicalQName(arg, "", env.getNamespaceResolver)
      new QNameValue(qn)
    } catch {
      case err: XPathException ⇒
        if (err.getErrorCodeQName == null) {
          err.setErrorCode("FONS0004")
        }
        throw err
    }
  }
}

/**
 * Cast Expression: implements "cast as data-type ( expression )". It also allows an internal
 * cast, which has the same semantics as a user-requested cast, but maps an empty sequence to
 * an empty sequence.
 */
class CastExpression(source: Expression, @BeanProperty var targetType: AtomicType, var allowEmpty: Boolean)
    extends UnaryExpression(source) {

  private var upcast: Boolean = false

  adoptChildExpression(source)

  /**
   * Simplify the expression
   * @return the simplified expression
   * @param visitor an expression visitor
   */
  override def simplify(visitor: ExpressionVisitor): Expression = {
    operand = visitor.simplify(operand)
    if (Literal.isAtomic(operand)) {
      return typeCheck(visitor, Type.ITEM_TYPE)
    }
    this
  }

  /**
   * Type-check the expression
   */
  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    operand = visitor.typeCheck(operand, contextItemType)
    val atomicType = SequenceType.makeSequenceType(AtomicType.ANY_ATOMIC, getCardinality)
    val role = new RoleLocator(RoleLocator.TYPE_OP, "cast as", 0)
    operand = TypeChecker.staticTypeCheck(operand, atomicType, backwardsCompatible = false, role)
    val th = TypeHierarchy.getInstance
    val sourceType = operand.getItemType
    val r = th.relationship(sourceType, targetType)
    if (r == TypeHierarchy.SAME_TYPE) {
      return operand
    } else if (r == TypeHierarchy.SUBSUMED_BY) {
      upcast = true
      return this
    }
    operand match {
      case literal: Literal ⇒
        val literalOperand = literal.getValue
        if (literalOperand.isInstanceOf[AtomicValue]) {
          val av = evaluateItem(new EarlyEvaluationContext(visitor.getConfiguration)).asInstanceOf[AtomicValue]
          return Literal.makeLiteral(av)
        }
        if (literalOperand.isInstanceOf[EmptySequence]) {
          if (allowEmpty) {
            return operand
          } else {
            typeError("Cast can never succeed: the operand must not be an empty sequence", "XPTY0004")
          }
        }
      case _ ⇒
    }
    if (sourceType != EmptySequenceTest.getInstance) {
      val p = sourceType.getAtomizedItemType
      if (!isPossibleCast(p, targetType)) {
        typeError("Casting from " + sourceType + " to " + targetType + " can never succeed", "XPTY0004")
      }
    }
    this
  }

  /**
   * Perform optimisation of an expression and its subexpressions.
   * <p/>
   * <p>This method is called after all references to functions and variables have been resolved
   * to the declaration of the function or variable, and after all type checking has been done.</p>
   * @param visitor         an expression visitor
   * @param contextItemType the static type of "." at the point where this expression is invoked.
   *                        The parameter is set to null if it is known statically that the context item will be undefined.
   *                        If the type of the context item is not known statically, the argument is set to
   *                        [[org.orbeon.darius.xpath.type.Type.ITEM_TYPE]]
   * @return the original expression, rewritten if appropriate to optimize execution
   * @throws org.orbeon.darius.xpath.trans.XPathException
   *          if an error is discovered during this phase
   *          (typically a type error)
   */
  override def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val th = TypeHierarchy.getInstance
    val e2 = super.optimize(visitor, contextItemType)
    if (e2 != this) {
      return e2
    }
    if (targetType == AtomicType.UNTYPED_ATOMIC) {
      operand match {
        case stringFn: StringFn ⇒
          val e = stringFn.getArguments(0)
          if (e.getItemType.isInstanceOf[AtomicType] && e.getCardinality == StaticProperty.EXACTLY_ONE) {
            operand = e
          }
        case _ ⇒
      }
    }
    operand match {
      case stringFn: StringFn ⇒
        val e = stringFn.getArguments(0)
        val et = e.getItemType
        if (et.isInstanceOf[AtomicType] && e.getCardinality == StaticProperty.EXACTLY_ONE &&
          th.isSubType(et, targetType)) {
          return e
        }
      case _ ⇒
    }
    operand match {
      case castExpression: CastExpression ⇒
        val it = castExpression.targetType
        if (th.isSubType(it, AtomicType.STRING) || th.isSubType(it, AtomicType.UNTYPED_ATOMIC)) {
          val e = castExpression.getBaseExpression
          val et = e.getItemType
          if (et.isInstanceOf[AtomicType] && e.getCardinality == StaticProperty.EXACTLY_ONE &&
            th.isSubType(et, targetType)) {
            return e
          }
        }
      case _ ⇒
    }
    if (!Cardinality.allowsZero(operand.getCardinality)) {
      allowEmpty = false
      resetLocalStaticProperties()
    }
    this
  }

  /**
   * Get the static cardinality of the expression
   */
  override def computeCardinality(): Int = {
    if (allowEmpty && Cardinality.allowsZero(operand.getCardinality)) StaticProperty.ALLOWS_ZERO_OR_ONE else StaticProperty.EXACTLY_ONE
  }

  /**
   * Get the static type of the expression
   */
  override def getItemType: ItemType = targetType

  /**
   * Determine the special properties of this expression
   * @return [[StaticProperty.NON_CREATIVE]].
   */
  override def computeSpecialProperties(): Int = {
    val p = super.computeSpecialProperties()
    p | StaticProperty.NON_CREATIVE
  }

  /**
   * Evaluate the expression
   */
  override def evaluateItem(context: XPathContext): Item = {
    val value = operand.evaluateItem(context).asInstanceOf[AtomicValue]
    if (value == null) {
      if (allowEmpty) {
        return null
      } else {
        throw new XPathException("Cast does not allow an empty sequence", "XPTY0004", getSourceLocator)
      }
    }
    if (upcast) {
      return value.convert(targetType).asInstanceOf[AtomicValue]
    }
    val result = value.convert(targetType)
    result match {
      case err: ValidationFailure ⇒
        val code = err.getErrorCodeQName
        var lcode = if (code == null) null else code.getLocalName
        if (lcode == null) {
          lcode = "FORG0001"
        }
        dynamicError(err.getMessage, lcode)
        return null
      case _ ⇒
    }
    result.asInstanceOf[AtomicValue]
  }

  /**
   * Is this expression the same as another expression?
   */
  override def equals(other: Any): Boolean = {
    super.equals(other) &&
      targetType == other.asInstanceOf[CastExpression].targetType &&
      allowEmpty == other.asInstanceOf[CastExpression].allowEmpty
  }

  /**
   * get HashCode for comparing two expressions. Note that this hashcode gives the same
   * result for (A op B) and for (B op A), whether or not the operator is commutative.
   */
  override def hashCode(): Int = super.hashCode ^ targetType.hashCode

  /**
   * The toString() method for an expression attempts to give a representation of the expression
   * in an XPath-like form, but there is no guarantee that the syntax will actually be true XPath.
   * In the case of XSLT instructions, the toString() method gives an abstracted view of the syntax
   */
  override def toString: String = {
    targetType.toString + "(" + operand.toString + ")"
  }
}
