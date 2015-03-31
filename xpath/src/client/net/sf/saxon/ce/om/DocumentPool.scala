// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.om

import client.net.sf.saxon.ce.orbeon._

/**
 * An object representing the collection of documents handled during
 * a single transformation.
 *
 * <p>The function of allocating document numbers is performed
 * by the DocumentNumberAllocator in the Configuration, not by the DocumentPool. This has a
 * number of effects: in particular it allows operations involving multiple
 * documents (such as generateId() and document()) to occur in a free-standing
 * XPath environment.</p>
 */
class DocumentPool {

  private val documentNameMap: Map[DocumentURI, DocumentInfo] = new HashMap[DocumentURI, DocumentInfo](10)

  private val unavailableDocuments: Set[DocumentURI] = new HashSet[DocumentURI](10)

  /**
   * Add a document to the pool
   * @param doc The DocumentInfo for the document in question
   * @param uri The document-uri property of the document.
   */
  def add(doc: DocumentInfo, uri: String): Unit = {
    if (uri != null) {
      documentNameMap.put(new DocumentURI(uri), doc)
    }
  }

  /**
   * Add a document to the pool
   * @param doc The DocumentInfo for the document in question
   * @param uri The document-uri property of the document.
   */
  def add(doc: DocumentInfo, uri: DocumentURI): Unit = {
    if (uri != null) {
      documentNameMap.put(uri, doc)
    }
  }

  /**
   * Get the document with a given document-uri
   * @param uri The document-uri property of the document.
   * @return the DocumentInfo with the given document-uri property if it exists,
   * or null if it is not found.
   */
  def find(uri: String): DocumentInfo = {
    documentNameMap.get(new DocumentURI(uri))
  }

  /**
   * Get the document with a given document-uri
   * @param uri The document-uri property of the document.
   * @return the DocumentInfo with the given document-uri property if it exists,
   * or null if it is not found.
   */
  def find(uri: DocumentURI): DocumentInfo = documentNameMap.get(uri)

  /**
   * Get the URI for a given document node, if it is present in the pool. This supports the
   * document-uri() function.
   * @param doc The document node
   * @return The uri of the document node, if present in the pool, or the systemId of the document node otherwise
   */
  def getDocumentURI(doc: NodeInfo): String = {
    val iter = documentNameMap.keysIterator()
    while (iter.hasNext) {
      val uri = iter.next()
      if (find(uri).isSameNodeInfo(doc)) {
        return uri.toString
      }
    }
    null
  }

  /**
   * Determine whether a given document is present in the pool
   * @param doc the document being sought
   * @return true if the document is present, false otherwise
   */
  def contains(doc: DocumentInfo): Boolean = documentNameMap.containsValue(doc)

  /**
   * Release a document from the document pool. This means that if the same document is
   * loaded again later, the source will need to be re-parsed, and nodes will get new identities.
   * @param doc the document to be discarded from the pool
   * @return the document supplied in the doc parameter
   */
  def discard(doc: DocumentInfo): DocumentInfo = {
    for ((name, entry) ← documentNameMap) {
      if (entry.isSameNodeInfo(doc)) {
        documentNameMap.remove(name)
        return doc
      }
    }
    doc
  }

  /**
   * Add a document URI to the set of URIs known to be unavailable (because doc-available() has returned
   * false
   * @param uri the URI of the unavailable document
   */
  def markUnavailable(uri: DocumentURI): Unit = {
    unavailableDocuments.add(uri)
  }

  /**
   * Ask whether a document URI is in the set of URIs known to be unavailable, because doc-available()
   * has been previously called and has returned false
   */
  def isMarkedUnavailable(uri: DocumentURI): Boolean = unavailableDocuments.contains(uri)
}
