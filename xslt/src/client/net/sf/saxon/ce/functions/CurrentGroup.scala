// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.StaticProperty
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.expr.sort.GroupIterator
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.tree.iter.EmptyIterator
import client.net.sf.saxon.ce.trans.XPathException

import scala.collection.JavaConversions._

/**
 * Implements the XSLT function current-group()
 */
class CurrentGroup extends SystemFunction {

  def newInstance(): CurrentGroup = new CurrentGroup()

  /**
   * Determine the dependencies
   */
  def getIntrinsicDependencies(): Int = StaticProperty.DEPENDS_ON_CURRENT_GROUP

  /**
   * Return an iteration over the result sequence
   */
  def iterate(c: XPathContext): SequenceIterator = {
    val gi = c.getCurrentGroupIterator
    if (gi == null) {
      return EmptyIterator.getInstance
    }
    gi.iterateCurrentGroup()
  }
}
