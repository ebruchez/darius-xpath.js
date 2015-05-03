// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr.sort

import org.orbeon.darius.xpath.expr.XPathContext
import org.orbeon.darius.xpath.om.NodeInfo
import org.orbeon.darius.xpath.om.SequenceIterator
import org.orbeon.darius.xpath.pattern.Pattern
import org.orbeon.darius.xpath.trans.XPathException
import java.util.ArrayList

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

  protected def advance(): Unit = {
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

  def getAnother: SequenceIterator = {
    new GroupStartingIterator(population.getAnother, pattern, baseContext)
  }
}
