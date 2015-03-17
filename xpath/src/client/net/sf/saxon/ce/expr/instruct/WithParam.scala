// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.om.Sequence

//ORBEON placeholder
trait WithParam {
  def getParameterId: Int
  def getSelectValue(context: XPathContext): Sequence
  def isTypeChecked: Boolean
}