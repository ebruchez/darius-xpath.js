package client.net.sf.saxon.ce.expr.sort

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.pattern.Pattern
import client.net.sf.saxon.ce.trans.XPathException
import java.util.ArrayList
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A GroupEndingIterator iterates over a sequence of groups defined by
 * xsl:for-each-group group-ending-with="x". The groups are returned in
 * order of first appearance.
 */
class GroupEndingIterator(population: SequenceIterator, endPattern: Pattern, context: XPathContext)
    extends GroupMatchingIterator with GroupIterator {

  this.pattern = endPattern

  baseContext = context

  runningContext = context.newMinorContext()

  this.population = runningContext.setCurrentIterator(population)

  next = population.next()

  protected def advance() {
    currentMembers = new ArrayList(20)
    currentMembers.add(current)
    next = current
    while (next != null) {
      if (pattern.matches(next.asInstanceOf[NodeInfo], runningContext)) {
        next = population.next()
        if (next != null) {
          //break
        }
      } else {
        next = population.next()
        if (next != null) {
          currentMembers.add(next)
        }
      }
    }
  }

  def getAnother(): SequenceIterator = {
    new GroupEndingIterator(population.getAnother, pattern, baseContext)
  }
}
