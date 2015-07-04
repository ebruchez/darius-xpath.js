// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.`type`

import java.{util ⇒ ju}

import org.orbeon.darius.xpath.`type`.TypeHierarchy._
import org.orbeon.darius.xpath.pattern.{AnyNodeTest, EmptySequenceTest, NameTest, NodeTest}

object TypeHierarchy {

  val  getInstance = new TypeHierarchy

  /**
   * Constant denoting relationship between two types: A is the same type as B
   */
  val SAME_TYPE = 0

  /**
   * Constant denoting relationship between two types: A subsumes B
   */
  val SUBSUMES = 1

  /**
   * Constant denoting relationship between two types: A is subsumed by B
   */
  val SUBSUMED_BY = 2

  /**
   * Constant denoting relationship between two types: A overlaps B
   */
  val OVERLAPS = 3

  /**
   * Constant denoting relationship between two types: A is disjoint from B
   */
  val DISJOINT = 4

  private class ItemTypePair(var s: ItemType, var t: ItemType) {

    /**
     * Returns a hash code value for the object.
     * @return a hash code value for this object.
     * @see Object#equals(Object)
     * @see java.util.Hashtable
     */
    override def hashCode(): Int = s.hashCode ^ t.hashCode

    /**
     * Indicates whether some other object is "equal to" this one.
     */
    override def equals(obj: Any): Boolean = obj match {
      case obj: ItemTypePair ⇒
        val pair = obj
        s == pair.s && t == pair.t
      case _ ⇒ false
    }
  }
}

/**
 * This class exists to provide answers to questions about the type hierarchy. Because
 * such questions are potentially expensive, it caches the answers.
 *
 * <p>In Saxon-CE, because the number of types is bounded, and the same for all applications,
 * the TypeHierarchy cache is in static data.</p>
 */
class TypeHierarchy {

  private val map: ju.Map[ItemTypePair, Integer] = new ju.HashMap[ItemTypePair, Integer]()

  /**
   * Determine whether type A is type B or one of its subtypes, recursively
   *
   * @param subtype identifies the first type
   * @param supertype identifies the second type
   * @return true if the first type is the second type or a (direct or
   *     indirect) subtype of the second type
   */
  def isSubType(subtype: ItemType, supertype: ItemType): Boolean = {
    val relation = relationship(subtype, supertype)
    relation == SAME_TYPE || relation == SUBSUMED_BY
  }

  /**
   * Determine the relationship of one item type to another.
   * @param t1 the first item type
   * @param t2 the second item type
   * @return [[SAME_TYPE]] if the types are the same; [[SUBSUMES]] if the first
   * type subsumes the second (that is, all instances of the second type are also instances
   * of the first); [[SUBSUMED_BY]] if the second type subsumes the first;
   * [[OVERLAPS]] if the two types overlap (have a non-empty intersection, but neither
   * subsumes the other); [[DISJOINT]] if the two types are disjoint (have an empty intersection)
   */
  def relationship(t1: ItemType, t2: ItemType): Int = {
    if (t1 == null) {
      throw new NullPointerException()
    }
    if (t1 == t2) {
      return SAME_TYPE
    }
    val pair = new ItemTypePair(t1, t2)
    var result = map.get(pair)
    if (result == null) {
      result = computeRelationship(t1, t2)
      map.put(pair, result)
    }
    result
  }

  /**
   * Determine the relationship of one item type to another.
   * @param t1 the first item type
   * @param t2 the second item type
   * @return [[SAME_TYPE]] if the types are the same; [[SUBSUMES]] if the first
   * type subsumes the second (that is, all instances of the second type are also instances
   * of the first); [[SUBSUMED_BY]] if the second type subsumes the first;
   * [[OVERLAPS]] if the two types overlap (have a non-empty intersection, but neither
   * subsumes the other); [[DISJOINT]] if the two types are disjoint (have an empty intersection)
   */
  private def computeRelationship(t1: ItemType, t2: ItemType): Int = {
    if (t1 == t2) {
      return SAME_TYPE
    }
    if (t1.isInstanceOf[AnyItemType]) {
      if (t2.isInstanceOf[AnyItemType]) {
        SAME_TYPE
      } else {
        SUBSUMES
      }
    } else if (t2.isInstanceOf[AnyItemType]) {
      SUBSUMED_BY
    } else if (t1.isInstanceOf[AtomicType]) {
      if (t2.isInstanceOf[NodeTest]) {
        DISJOINT
      } else {
        var t = t2
        while (t.isInstanceOf[AtomicType]) {
          if (t1 == t) {
            return SUBSUMES
          }
          t = t.getSuperType
        }
        t = t1
        while (t.isInstanceOf[AtomicType]) {
          if (t == t2) {
            return SUBSUMED_BY
          }
          t = t.getSuperType
        }
        DISJOINT
      }
    } else if (t1.isInstanceOf[NodeTest]) {
      if (t2.isInstanceOf[AtomicType]) {
        DISJOINT
      } else {
        if (t1.isInstanceOf[AnyNodeTest]) {
          if (t2.isInstanceOf[AnyNodeTest]) {
            SAME_TYPE
          } else {
            SUBSUMES
          }
        } else if (t2.isInstanceOf[AnyNodeTest]) {
          SUBSUMED_BY
        } else if (t1.isInstanceOf[EmptySequenceTest]) {
          DISJOINT
        } else if (t2.isInstanceOf[EmptySequenceTest]) {
          DISJOINT
        } else {
          val m1 = t1.asInstanceOf[NodeTest].getNodeKindMask
          val m2 = t2.asInstanceOf[NodeTest].getNodeKindMask

          if ((m1 & m2) == 0) {
            return DISJOINT
          }

          val nodeKindRelationship =
              if (m1 == m2)
                SAME_TYPE
              else if ((m1 & m2) == m1)
                SUBSUMED_BY
              else if ((m1 & m2) == m2)
                SUBSUMES
              else
                OVERLAPS

          val n1 = t1 match {
            case nameTest: NameTest ⇒ nameTest.getRequiredNodeName
            case _ ⇒ null
          }
          val n2 = t2 match {
            case nameTest: NameTest ⇒ nameTest.getRequiredNodeName
            case _ ⇒ null
          }

          val nodeNameRelationship =
            if (n1 == null) {
              if (n2 == null) SAME_TYPE else SUBSUMES
            } else if (n2 == null) {
              SUBSUMED_BY
            } else if (n1 == n2) {
              SAME_TYPE
            } else {
              DISJOINT
            }

          if (nodeKindRelationship == SAME_TYPE && nodeNameRelationship == SAME_TYPE) {
            SAME_TYPE
          } else if ((nodeKindRelationship == SAME_TYPE || nodeKindRelationship == SUBSUMES) &&
            (nodeNameRelationship == SAME_TYPE || nodeNameRelationship == SUBSUMES)) {
            SUBSUMES
          } else if ((nodeKindRelationship == SAME_TYPE || nodeKindRelationship == SUBSUMED_BY) &&
            (nodeNameRelationship == SAME_TYPE || nodeNameRelationship == SUBSUMED_BY)) {
            SUBSUMED_BY
          } else if (nodeNameRelationship == DISJOINT) {
            DISJOINT
          } else {
            OVERLAPS
          }
        }
      }
    } else {
      DISJOINT
    }
  }
}
