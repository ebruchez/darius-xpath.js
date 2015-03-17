// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.trans

import client.net.sf.saxon.ce.expr.instruct.Template
import client.net.sf.saxon.ce.pattern.Pattern
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Rule: a template rule, or a strip-space rule used to support the implementation
 */
class Rule(@BeanProperty var pattern: Pattern, 
    @BeanProperty var action: Template, 
    @BeanProperty var precedence: Int, 
    @BeanProperty var minImportPrecedence: Int, 
    @BeanProperty var priority: Double, 
    @BeanProperty var sequence: Int, 
    @BeanProperty var ixslPreventDefault: Boolean, 
    var ixslEventProperty: String) {

  @BeanProperty
  var next: Rule = null

  @BooleanBeanProperty
  var alwaysMatches: Boolean = _

  @BeanProperty
  var rank: Int = _

  var isVirtual: Boolean = false

  /**
   * Copy a rule, including the chain of rules linked to it
   * @param r the rule to be copied
   */
  def this(r: Rule) {
    this()
    pattern = r.pattern
    action = r.action
    precedence = r.precedence
    minImportPrecedence = r.minImportPrecedence
    priority = r.priority
    sequence = r.sequence
    ixslPreventDefault = r.ixslPreventDefault
    next = if (r.next == null) null else new Rule(r.next)
  }

  def setIsVirtual() {
    isVirtual = true
  }

  def getEventProperty(): String = ixslEventProperty

  /**
   * Rules have an ordering, based on their precedence and priority. This method compares
   * them using the precomputed rank value.
   * @param other Another rule whose ordering rank is to be compared with this one
   * @return <0 if this rule has lower rank, that is if it has lower precedence or equal
   * precedence and lower priority. 0 if the two rules have equal precedence and
   * priority. >0 if this rule has higher rank in precedence/priority order
   */
  def compareRank(other: Rule): Int = rank - other.rank

  /**
   * Rules have an ordering, based on their precedence and priority.
   * @param other Another rule whose ordering rank is to be compared with this one
   * @return <0 if this rule has lower rank, that is if it has lower precedence or equal
   * precedence and lower priority. 0 if the two rules have equal precedence and
   * priority. >0 if this rule has higher rank in precedence/priority order
   */
  def compareComputedRank(other: Rule): Int = {
    if (precedence == other.precedence) {
      if (priority == other.priority) {
        0
      } else if (priority < other.priority) {
        -1
      } else {
        +1
      }
    } else if (precedence < other.precedence) {
      -1
    } else {
      +1
    }
  }
}
