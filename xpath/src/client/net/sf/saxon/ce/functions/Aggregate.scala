// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.{ExpressionTool, ExpressionVisitor}

/**
 * This abstract class provides functionality common to the sum(), avg(), count(),
 * exists(), and empty() functions. These all take a sequence as input and produce a singleton
 * as output; and they are all independent of the ordering of the items in the input.
 */
abstract class Aggregate extends SystemFunction {

  /**
   * Static analysis: prevent sorting of the argument
   */
  override def checkArguments(visitor: ExpressionVisitor): Unit = {
    super.checkArguments(visitor)
    argument(0) = ExpressionTool.unsorted(visitor.getConfiguration, argument(0), true)
  }
}
