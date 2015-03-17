// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.value

import client.net.sf.saxon.ce.expr.StaticProperty
import client.net.sf.saxon.ce.pattern.AnyNodeTest
import client.net.sf.saxon.ce.pattern.EmptySequenceTest
import client.net.sf.saxon.ce.`type`.AnyItemType
import client.net.sf.saxon.ce.`type`.AtomicType
import client.net.sf.saxon.ce.`type`.ItemType
import SequenceType._
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

object SequenceType {

  /**
   * A type that allows any sequence of items
   */
  val ANY_SEQUENCE = makeSequenceType(AnyItemType.getInstance, StaticProperty.ALLOWS_ZERO_OR_MORE)

  /**
   * A type that allows exactly one item, of any kind
   */
  val SINGLE_ITEM = makeSequenceType(AnyItemType.getInstance, StaticProperty.EXACTLY_ONE)

  /**
   * A type that allows zero or one items, of any kind
   */
  val OPTIONAL_ITEM = makeSequenceType(AnyItemType.getInstance, StaticProperty.ALLOWS_ZERO_OR_ONE)

  /**
   * A type that allows exactly one atomic value
   */
  val SINGLE_ATOMIC = makeSequenceType(AtomicType.ANY_ATOMIC, StaticProperty.EXACTLY_ONE)

  /**
   * A type that allows zero or one atomic values
   */
  val OPTIONAL_ATOMIC = makeSequenceType(AtomicType.ANY_ATOMIC, StaticProperty.ALLOWS_ZERO_OR_ONE)

  /**
   * A type that allows zero or more atomic values
   */
  val ATOMIC_SEQUENCE = makeSequenceType(AtomicType.ANY_ATOMIC, StaticProperty.ALLOWS_ZERO_OR_MORE)

  /**
   * A type that allows a single string
   */
  val SINGLE_STRING = makeSequenceType(AtomicType.STRING, StaticProperty.EXACTLY_ONE)

  /**
   * A type that allows a single untyped atomic
   */
  val SINGLE_UNTYPED_ATOMIC = makeSequenceType(AtomicType.UNTYPED_ATOMIC, StaticProperty.EXACTLY_ONE)

  /**
   * A type that allows a single optional integer
   */
  val OPTIONAL_INTEGER = makeSequenceType(AtomicType.INTEGER, StaticProperty.ALLOWS_ZERO_OR_ONE)

  val SINGLE_INTEGER = makeSequenceType(AtomicType.INTEGER, StaticProperty.EXACTLY_ONE)

  /**
   * A type that allows an optional numeric value
   */
  val OPTIONAL_NUMERIC = makeSequenceType(AtomicType.NUMERIC, StaticProperty.ALLOWS_ZERO_OR_ONE)

  /**
   * A type that allows zero or one nodes
   */
  val OPTIONAL_NODE = makeSequenceType(AnyNodeTest.getInstance, StaticProperty.ALLOWS_ZERO_OR_ONE)

  val SINGLE_NODE = makeSequenceType(AnyNodeTest.getInstance, StaticProperty.EXACTLY_ONE)

  val NODE_SEQUENCE = makeSequenceType(AnyNodeTest.getInstance, StaticProperty.ALLOWS_ZERO_OR_MORE)

  /**
   * Construct an instance of SequenceType.
   *
   * @param primaryType The item type
   * @param cardinality The required cardinality
   * @return the corresponding sequence type
   */
  def makeSequenceType(primaryType: ItemType, cardinality: Int): SequenceType = {
    new SequenceType(primaryType, cardinality)
  }
}

/**
 * SequenceType: a sequence type consists of a primary type, which indicates the type of item,
 * and a cardinality, which indicates the number of occurrences permitted. Where the primary type
 * is element or attribute, there may also be a content type, indicating the required type
 * annotation on the element or attribute content.
 */
class SequenceType private (@BeanProperty var primaryType: ItemType, cardinality: Int)
    {

  @BeanProperty
  var cardinality: Int = if (primaryType.isInstanceOf[EmptySequenceTest]) StaticProperty.EMPTY else cardinality

  /**
   * Return a string representation of this SequenceType
   *
   * @return the string representation as an instance of the XPath
   *         SequenceType construct
   */
  override def toString(): String = {
    var s = primaryType.toString
    if (cardinality == StaticProperty.ALLOWS_ONE_OR_MORE) {
      s = s + '+'
    } else if (cardinality == StaticProperty.ALLOWS_ZERO_OR_MORE) {
      s = s + '*'
    } else if (cardinality == StaticProperty.ALLOWS_ZERO_OR_ONE) {
      s = s + '?'
    }
    s
  }

  /**
   * Returns a hash code value for the object.
   */
  override def hashCode(): Int = primaryType.hashCode ^ cardinality

  /**
   * Indicates whether some other object is "equal to" this one.
   */
  override def equals(obj: Any): Boolean = {
    obj.isInstanceOf[SequenceType] && 
      this.primaryType == obj.asInstanceOf[SequenceType].primaryType && 
      this.cardinality == obj.asInstanceOf[SequenceType].cardinality
  }
}
