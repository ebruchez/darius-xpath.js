// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.expr

import org.orbeon.darius.xpath.om.SequenceIterator

/**
 * MappingFunction is an interface that must be satisfied by an object passed to a
 * MappingIterator. StatefulMappingFunction is a sub-interface representing a mapping
 * function that maintains state information, and which must therefore be cloned
 * when the mapping iterator is cloned.
 */
trait StatefulMappingFunction {

  /**
   * Return a clone of this MappingFunction, with the state reset to its state at the beginning
   * of the underlying iteration
   * @return a clone of this MappingFunction
   * @param newBaseIterator the cloned iterator to which the mapping function will be applied
   */
  def getAnother(newBaseIterator: SequenceIterator): StatefulMappingFunction
}
