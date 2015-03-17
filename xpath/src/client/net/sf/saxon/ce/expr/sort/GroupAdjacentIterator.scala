package client.net.sf.saxon.ce.expr.sort

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.functions.DistinctValues
import client.net.sf.saxon.ce.lib.StringCollator
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.FocusIterator
import client.net.sf.saxon.ce.tree.iter.ListIterator
import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.value.AtomicValue
import java.util.ArrayList
import java.util.List
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A GroupAdjacentIterator iterates over a sequence of groups defined by
 * xsl:for-each-group group-adjacent="x". The groups are returned in
 * order of first appearance.
 * <p>
 * Each step of this iterator advances to the first item of the next group,
 * leaving the members of that group in a saved list.
 */
class GroupAdjacentIterator(population: SequenceIterator, 
    var keyExpression: Expression, 
    var baseContext: XPathContext, 
    var collator: StringCollator) extends GroupIterator {

  private var population: FocusIterator = runningContext.setCurrentIterator(population)

  private var currentComparisonKey: AnyRef = _

  private var runningContext: XPathContext = baseContext.newMinorContext()

  private var currentKey: AtomicValue = null

  private var currentMembers: List[Item] = _

  private var nextKey: AtomicValue = null

  private var next: Item = this.population.next()

  var current: Item = null

  private var position: Int = 0

  if (next != null) {
    nextKey = keyExpression.evaluateItem(runningContext).asInstanceOf[AtomicValue]
  }

  private def advance() {
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
          next = nextCandidate
          nextKey = candidateKey
          return
        }
      } catch {
        case e: ClassCastException => {
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
    next = null
    nextKey = null
  }

  private def comparisonKey(candidateKey: AtomicValue): AnyRef = {
    if (candidateKey.isNaN) {
      classOf[DistinctValues]
    } else {
      candidateKey.getXPathComparable(false, collator, baseContext.getImplicitTimezone)
    }
  }

  def getCurrentGroupingKey(): AtomicValue = currentKey

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

  def getAnother(): SequenceIterator = {
    new GroupAdjacentIterator(population.getAnother, keyExpression, baseContext, collator)
  }
}
