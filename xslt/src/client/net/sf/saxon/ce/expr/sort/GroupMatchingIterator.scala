// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr.sort

import org.orbeon.darius.xpath.expr.XPathContext
import org.orbeon.darius.xpath.om.Item
import org.orbeon.darius.xpath.om.SequenceIterator
import org.orbeon.darius.xpath.pattern.Pattern
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.tree.iter.ListIterator
import org.orbeon.darius.xpath.value.AtomicValue
import java.util.List

import scala.collection.JavaConversions._

/**
 * A GroupMatchingIterator contains code shared between GroupStartingIterator and GroupEndingIterator
 */
abstract class GroupMatchingIterator extends GroupIterator {

  protected var population: SequenceIterator = _

  protected var pattern: Pattern = _

  protected var baseContext: XPathContext = _

  protected var runningContext: XPathContext = _

  protected var currentMembers: List[_] = _

  protected var next: Item = _

  protected var current: Item = null

  protected var position: Int = 0

  protected def advance(): Unit

  def getCurrentGroupingKey: AtomicValue = null

  def iterateCurrentGroup(): SequenceIterator = new ListIterator(currentMembers)

  def next(): Item = {
    if (next != null) {
      current = next
      position += 1
      advance()
      current
    } else {
      current = null
      position = -1
      null
    }
  }

  def current(): Item = current

  private def position(): Int = position
}
