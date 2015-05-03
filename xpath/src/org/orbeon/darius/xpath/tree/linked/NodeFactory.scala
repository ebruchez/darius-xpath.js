// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.tree.linked

import org.orbeon.darius.xpath.event.PipelineConfiguration
import org.orbeon.darius.xpath.om.AttributeCollection
import org.orbeon.darius.xpath.om.NamespaceBinding
import org.orbeon.darius.xpath.om.NodeInfo
import org.orbeon.darius.xpath.om.StructuredQName

/**
 * Interface NodeFactory.
 * A Factory for nodes used to build a tree.
 * Currently only allows Element nodes to be user-constructed.
 * @author Michael H. Kay
 */
trait NodeFactory {

  /**
   * Create an Element node
   * @param parent The parent element
   * @param nameCode The element name
   * @param attlist The attribute collection, excluding any namespace attributes
   * @param namespaces List of new namespace declarations for this element, as a sequence
   * of namespace codes representing pairs of strings: (prefix1, uri1), (prefix2, uri2)...
   * @param namespacesUsed the number of elemnts of the namespaces array actually used
   * @param pipe The pipeline configuration (provides access to the error listener and the
   * location provider)
   * @param baseURI Indicates the source document base URI
   * @param sequenceNumber Sequence number to be assigned to represent document order.
   */
  def makeElementNode(
    parent: NodeInfo, 
    nameCode: StructuredQName, 
    attlist: AttributeCollection, 
    namespaces: Array[NamespaceBinding], 
    namespacesUsed: Int, 
    pipe: PipelineConfiguration, 
    baseURI: String, 
    sequenceNumber: Int
  ): ElementImpl
}
