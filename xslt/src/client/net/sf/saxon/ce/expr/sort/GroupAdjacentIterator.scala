// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.sort

import java.util.{ArrayList, List}

import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.expr.{Expression, XPathContext}
import client.net.sf.saxon.ce.functions.DistinctValues
import client.net.sf.saxon.ce.lib.StringCollator
import client.net.sf.saxon.ce.om.{Item, SequenceIterator}
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.{FocusIterator, ListIterator}
import client.net.sf.saxon.ce.value.AtomicValue

/**
 * A GroupAdjacentIterator iterates over a sequence of groups defined by
 * xsl:for-each-group group-adjacent="x". The groups are returned in
 * order of first appearance.
 * <p>
 * Each step of this iterator advances to the first item of the next group,
 * leaving the members of that group in a saved list.
 */
class GroupAdjacentIterator(_population: SequenceIterator,
    var keyExpression: Expression, 
    var baseContext: XPathContext, 
    var collator: StringCollator) extends GroupIterator {

  private val population: FocusIterator = runningContext.setCurrentIterator(_population)

  private var currentComparisonKey: AnyRef = _

  private val runningContext: XPathContext = baseContext.newMinorContext()

  private var currentKey: AtomicValue = null

  private var currentMembers: List[Item] = _

  private var nextKey: AtomicValue = null

  private var _next: Item = this.population.next()

  var current: Item = null

  private var position: Int = 0

  if (_next != null) {
    nextKey = keyExpression.evaluateItem(runningContext).asInstanceOf[AtomicValue]
  }

  private def advance(): Unit = {
    currentMembers = new ArrayList(20)
    currentMembers.add(current)
    while (true) {
      val nextCandidate = population.next()
      if (nextCandidate == null) {
        //break
      }
      val candidateKey = keyExpression.evaluateItem(runningContext).asInstanceOf[AtomicValue]
      try {
        val compKey = comparisonKey(candidateKey)
        if (currentComparisonKey == compKey) {
          currentMembers.add(nextCandidate)
        } else {
          _next = nextCandidate
          nextKey = candidateKey
          return
        }
      } catch {
        case e: ClassCastException ⇒ {
          val err = new XPathException("Grouping key values are of non-comparable types (" + 
            Type.displayTypeName(currentKey) + 
            " and " + 
            Type.displayTypeName(candidateKey) + 
            ')')
          err.setIsTypeError(true)
          throw err
        }
      }
    }
    _next = null
    nextKey = null
  }

  private def comparisonKey(candidateKey: AtomicValue): AnyRef = {
    if (candidateKey.isNaN) {
      classOf[DistinctValues]
    } else {
      candidateKey.getXPathComparable(false, collator, baseContext.getImplicitTimezone)
    }
  }

  def getCurrentGroupingKey: AtomicValue = currentKey

  def iterateCurrentGroup(): SequenceIterator = new ListIterator(currentMembers)

  def next(): Item = {
    if (next == null) {
      current = null
      position = -1
      return null
    }
    current = next
    currentKey = nextKey
    currentComparisonKey = comparisonKey(currentKey)
    position += 1
    advance()
    current
  }

  def getAnother: SequenceIterator = {
    new GroupAdjacentIterator(population.getAnother, keyExpression, baseContext, collator)
  }
}
