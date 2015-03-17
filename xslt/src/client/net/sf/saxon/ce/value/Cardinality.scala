// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.value

import client.net.sf.saxon.ce.expr._
//remove if not needed
import scala.collection.JavaConversions._

object Cardinality {

  /**
   * Determine whether multiple occurrences are allowed
   * @param cardinality the cardinality of a sequence
   * @return true if the cardinality allows the sequence to contain more than one item
   */
  def allowsMany(cardinality: Int): Boolean = {
    (cardinality & StaticProperty.ALLOWS_MANY) != 0
  }

  /**
   * Determine whether empty sequence is allowed
   * @param cardinality the cardinality of a sequence
   * @return true if the cardinality allows the sequence to be empty
   */
  def allowsZero(cardinality: Int): Boolean = {
    (cardinality & StaticProperty.ALLOWS_ZERO) != 0
  }

  /**
   * Form the union of two cardinalities. The cardinality of the expression "if (c) then e1 else e2"
   * is the union of the cardinalities of e1 and e2.
   * @param c1 a cardinality
   * @param c2 another cardinality
   * @return the cardinality that allows both c1 and c2
   */
  def union(c1: Int, c2: Int): Int = {
    var r = c1 | c2
    if (r == 
      (StaticProperty.ALLOWS_MANY | StaticProperty.ALLOWS_ZERO)) r = StaticProperty.ALLOWS_ZERO_OR_MORE
    r
  }

  /**
   * Add two cardinalities
   * @param c1 the first cardinality
   * @param c2 the second cardinality
   * @return the cardinality of a sequence formed by concatenating the sequences whose cardinalities
   * are c1 and c2
   */
  def sum(c1: Int, c2: Int): Int = {
    if (c1 == StaticProperty.EMPTY) {
      return c2
    }
    if (c2 == StaticProperty.EMPTY) {
      return c1
    }
    val allowsZero = Cardinality.allowsZero(c1) && Cardinality.allowsZero(c2)
    StaticProperty.ALLOWS_ONE_OR_MORE | 
      (if (allowsZero) StaticProperty.ALLOWS_ZERO else 0)
  }

  /**
   * Test if one cardinality subsumes another. Cardinality c1 subsumes c2 if every option permitted
   * by c2 is also permitted by c1.
   * @param c1 a cardinality
   * @param c2 another cardinality
   * @return true if if every option permitted
   * by c2 is also permitted by c1.
   */
  def subsumes(c1: Int, c2: Int): Boolean = (c1 | c2) == c1

  /**
   * Multiply two cardinalities
   * @param c1 the first cardinality
   * @param c2 the second cardinality
   * @return the product of the cardinalities, that is, the cardinality of the sequence
   * "for $x in S1 return S2", where c1 is the cardinality of S1 and c2 is the cardinality of S2
   */
  def multiply(c1: Int, c2: Int): Int = {
    if (c1 == StaticProperty.EMPTY || c2 == StaticProperty.EMPTY) {
      return StaticProperty.EMPTY
    }
    if (c2 == StaticProperty.EXACTLY_ONE) {
      return c1
    }
    if (c1 == StaticProperty.EXACTLY_ONE) {
      return c2
    }
    if (c1 == StaticProperty.ALLOWS_ZERO_OR_ONE && c2 == StaticProperty.ALLOWS_ZERO_OR_ONE) {
      return StaticProperty.ALLOWS_ZERO_OR_ONE
    }
    if (c1 == StaticProperty.ALLOWS_ONE_OR_MORE && c2 == StaticProperty.ALLOWS_ONE_OR_MORE) {
      return StaticProperty.ALLOWS_ONE_OR_MORE
    }
    StaticProperty.ALLOWS_ZERO_OR_MORE
  }

  /**
   * Display the cardinality as a string
   * @param cardinality the cardinality value to be displayed
   * @return the representation as a string, for example "zero or one", "zero or more"
   *
   */
  def toString(cardinality: Int): String = cardinality match {
    case StaticProperty.ALLOWS_ZERO_OR_ONE => "zero or one"
    case StaticProperty.EXACTLY_ONE => "exactly one"
    case StaticProperty.ALLOWS_ZERO_OR_MORE => "zero or more"
    case StaticProperty.ALLOWS_ONE_OR_MORE => "one or more"
    case StaticProperty.EMPTY => "exactly zero"
    case StaticProperty.ALLOWS_MANY => "zero or more"
    case _ => "code " + cardinality
  }
}
