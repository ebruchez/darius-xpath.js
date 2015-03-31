// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.om.{Item, NodeInfo, Sequence, SequenceIterator}
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.{StringValue, _}

import scala.beans.BeanProperty

object Literal {

  /**
   * Create a literal as a wrapper around a Value (factory method)
   * @param value the value of this literal
   * @return the Literal
   */
  def makeLiteral(value: Sequence): Literal = {
    value match {
      case stringValue: StringValue ⇒
        new StringLiteral(stringValue)
      case _ ⇒
        new Literal(value)
    }
  }

  /**
   * Make an empty-sequence literal
   * @return a literal whose value is the empty sequence
   */
  def makeEmptySequence(): Literal = new Literal(EmptySequence.getInstance)

  /**
   * Test whether the literal wraps an atomic value. (Note, if this method returns false,
   * this still leaves the possibility that the literal wraps a sequence that happens to contain
   * a single atomic value).
   * @param exp an expression
   * @return if the expression is a literal and the literal wraps an AtomicValue
   */
  def isAtomic(exp: Expression): Boolean = {
    exp.isInstanceOf[Literal] && 
      exp.asInstanceOf[Literal].getValue.isInstanceOf[AtomicValue]
  }

  /**
   * Test whether the literal explicitly wraps an empty sequence. (Note, if this method returns false,
   * this still leaves the possibility that the literal wraps a sequence that happens to be empty).
   * @param exp an expression
   * @return if the expression is a literal and the literal wraps an AtomicValue
   */
  def isEmptySequence(exp: Expression): Boolean = {
    exp.isInstanceOf[Literal] && 
      exp.asInstanceOf[Literal].getValue.isInstanceOf[EmptySequence]
  }

  /**
   * Test if a literal represents the boolean value true
   * @param exp an expression
   * @param value true or false
   * @return if the expression is a literal and the literal represents the boolean value given in the
   * second argument
   */
  def isConstantBoolean(exp: Expression, value: Boolean): Boolean = {
    exp match {
      case literal: Literal ⇒
        val b = literal.getValue
        b.isInstanceOf[BooleanValue] && b.asInstanceOf[BooleanValue].getBooleanValue == value
      case _ ⇒
        false
    }
  }

  /**
   * Test if a literal represents the integer value 1
   * @param exp an expression
   * @return if the expression is a literal and the literal represents the integer value 1
   */
  def isConstantOne(exp: Expression): Boolean = {
    try {
      exp match {
        case literal: Literal ⇒
          val v = literal.getValue
          v.isInstanceOf[IntegerValue] && v.asInstanceOf[IntegerValue].intValue() == 1
        case _ ⇒
          false
      }
    } catch {
      case e: XPathException ⇒ false
    }
  }
}

/**
 * A Literal is an expression whose value is constant: it is a class that implements the [[Expression]]
 * interface as a wrapper around a [[client.net.sf.saxon.ce.value.SequenceTool]]. This may derive from an actual literal in an XPath expression
 * or query, or it may be the result of evaluating a constant subexpression such as true() or xs:date('2007-01-16')
 */
class Literal(@BeanProperty var value: Sequence) extends Expression {

  /**
   * TypeCheck an expression
   * @return for a Value, this always returns the value unchanged
   */
  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = this

  /**
   * Optimize an expression
   * @return for a Value, this always returns the value unchanged
   */
  override def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = this

  /**
   * Determine the data type of the items in the expression, if possible
   * @return for the default implementation: AnyItemType (not known)
   */
  def getItemType: ItemType = SequenceTool.getItemTypeOfValue(value)

  /**
   * Determine the cardinality
   */
  def computeCardinality(): Int = {
    value match {
      case _: EmptySequence ⇒
        return StaticProperty.EMPTY
      case _: AtomicValue ⇒
        return StaticProperty.EXACTLY_ONE
      case _ ⇒
    }
    try {
      val iter = value.iterate()
      val next = iter.next()
      if (next == null) {
        StaticProperty.EMPTY
      } else {
        if (iter.next() != null) {
          StaticProperty.ALLOWS_ONE_OR_MORE
        } else {
          StaticProperty.EXACTLY_ONE
        }
      }
    } catch {
      case err: XPathException ⇒ StaticProperty.ALLOWS_ZERO_OR_MORE
    }
  }

  /**
   * Compute the static properties of this expression (other than its type). For a
   * Value, the only special property is [[StaticProperty.NON_CREATIVE]].
   * @return the value [[StaticProperty.NON_CREATIVE]]
   */
  override def computeSpecialProperties(): Int = {
    if (getValue.isInstanceOf[EmptySequence]) {
      return StaticProperty.SPECIAL_PROPERTY_MASK & ~StaticProperty.HAS_SIDE_EFFECTS
    }
    StaticProperty.NON_CREATIVE
  }

  /**
   * Copy an expression. This makes a deep copy.
   *
   * @return the copy of the original expression
   */
  private def copy(): Expression = new Literal(value)

  /**
   * Determine which aspects of the context the expression depends on. The result is
   * a bitwise-or'ed value composed from constants such as StaticProperty.VARIABLES and
   * StaticProperty.CURRENT_NODE
   * @return for a Value, this always returns zero.
   */
  override def getDependencies: Int = 0

  /**
   * Return an Iterator to iterate over the values of a sequence. The value of every
   * expression can be regarded as a sequence, so this method is supported for all
   * expressions. This default implementation handles iteration for expressions that
   * return singleton values: for non-singleton expressions, the subclass must
   * provide its own implementation.
   *
   * @param context supplies the context for evaluation
   * @return a SequenceIterator that can be used to iterate over the result
   *         of the expression
   * @throws client.net.sf.saxon.ce.trans.XPathException
   *          if any dynamic error occurs evaluating the
   *          expression
   */
  override def iterate(context: XPathContext): SequenceIterator = value.iterate()

  /**
   * Evaluate as a singleton item (or empty sequence). Note: this implementation returns
   * the first item in the sequence. The method should not be used unless appropriate type-checking
   * has been done to ensure that the value will be a singleton.
   */
  override def evaluateItem(context: XPathContext): Item = {
    value match {
      case atomicValue: AtomicValue ⇒
        return atomicValue
      case _ ⇒
    }
    value.iterate().next()
  }

  /**
   * Process the value as an instruction, without returning any tail calls
   * @param context The dynamic context, giving access to the current node,
   * the current variables, etc.
   */
  override def process(context: XPathContext): Unit = {
    val iter = value.iterate()
    val out = context.getReceiver
    while (true) {
      val it = iter.next()
      if (it == null)
        return
      out.append(it, NodeInfo.ALL_NAMESPACES)
    }
  }

  override def evaluateAsString(context: XPathContext): CharSequence = {
    val value = evaluateItem(context).asInstanceOf[AtomicValue]
    if (value == null) return ""
    value.getStringValue
  }

  /**
   * Determine whether two literals are equal, when considered as expressions.
   * @param obj the other expression
   * @return true if the two literals are equal. The test here requires (a) identity in the
   * sense defined by XML Schema (same value in the same value space), and (b) identical type
   * annotations. For example the literal xs:int(3) is not equal (as an expression) to xs:short(3),
   * because the two expressions are not interchangeable.
   */
  override def equals(obj: Any): Boolean = {
    if (!obj.isInstanceOf[Literal]) {
      return false
    }
    val v0 = value
    val v1 = obj.asInstanceOf[Literal].value
    try {
      val i0 = v0.iterate()
      val i1 = v1.iterate()
      while (true) {
        val m0 = i0.next()
        val m1 = i1.next()
        if (m0 == null && m1 == null) {
          return true
        }
        if (m0 == null || m1 == null) {
          return false
        }
        val n0 = m0.isInstanceOf[NodeInfo]
        val n1 = m1.isInstanceOf[NodeInfo]
        if (n0 != n1) {
          return false
        }
        if (n0 && n1 && 
          !m0.asInstanceOf[NodeInfo].isSameNodeInfo(m1.asInstanceOf[NodeInfo])) {
          return false
        }
        m0 match {
          case stringValue: StringValue if m1.isInstanceOf[StringValue] ⇒
            if (!(stringValue.getStringValue == m1.asInstanceOf[StringValue].getStringValue)) {
              false
            }
          case atomicValue: AtomicValue if m1.isInstanceOf[AtomicValue] ⇒
            if (!n0 && !n1 &&
              (atomicValue != m1.asInstanceOf[AtomicValue]) ||
              atomicValue.getItemType != m1.asInstanceOf[AtomicValue].getItemType) {
              false
            }
          case _ ⇒
        }
      }
      throw new IllegalStateException
    } catch {
      case err: XPathException ⇒ false
    }
  }

  /**
   * Return a hash code to support the equals() function
   */
  override def hashCode(): Int = value.hashCode

  /**
   * The toString() method for an expression attempts to give a representation of the expression
   * in an XPath-like form, but there is no guarantee that the syntax will actually be true XPath.
   * In the case of XSLT instructions, the toString() method gives an abstracted view of the syntax
   */
  override def toString(): String = value.toString
}
