// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr

import org.orbeon.darius.xpath.expr.sort.AtomicComparer

/**
 * Interface implemented by expressions that perform a comparison
 */
trait ComparisonExpression {

  /**
   * Get the AtomicComparer used to compare atomic values. This encapsulates any collation that is used
   */
  def getAtomicComparer: AtomicComparer

  /**
   * Get the primitive (singleton) operator used: one of Token.FEQ, Token.FNE, Token.FLT, Token.FGT,
   * Token.FLE, Token.FGE
   */
  def getSingletonOperator: Int

  /**
   * Get the two operands of the comparison
   */
  def getOperands: Array[Expression]
}
