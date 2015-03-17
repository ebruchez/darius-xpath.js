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
 * A GroupStartingIterator iterates over a sequence of groups defined by
 * xsl:for-each-group group-starting-with="x". The groups are returned in
 * order of first appearance.
 */
class GroupStartingIterator(population: SequenceIterator, startPattern: Pattern, context: XPathContext)
    extends GroupMatchingIterator with GroupIterator {

  this.pattern = startPattern

  baseContext = context

  runningContext = context.newMinorContext()

  this.population = runningContext.setCurrentIterator(population)

  next = population.next()

  protected def advance() {
    currentMembers = new ArrayList(10)
    currentMembers.add(current)
    while (true) {
      val nextCandidate = population.next().asInstanceOf[NodeInfo]
      if (nextCandidate == null) {
        //break
      }
      if (pattern.matches(nextCandidate, runningContext)) {
        next = nextCandidate
        return
      } else {
        currentMembers.add(nextCandidate)
      }
    }
    next = null
  }

  def getAnother(): SequenceIterator = {
    new GroupStartingIterator(population.getAnother, pattern, baseContext)
  }
}
