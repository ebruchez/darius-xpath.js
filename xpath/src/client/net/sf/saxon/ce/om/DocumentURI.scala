// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.om

import client.net.sf.saxon.ce.om.DocumentURI._

object DocumentURI {

  val CASE_BLIND_FILES = false

  /**
   * Normalize the representation of file: URIs to give better equality matching than straight
   * string comparison. The main purpose is (a) to eliminate the distinction between "file:/" and
   * "file:///", and (b) to normalize case in the case of Windows filenames: especially the distinction
   * between "file:/C:" and "file:/c:".
   * @param uri the URI to be normalized
   * @return the normalized URI.
   */
  def normalizeURI(uri: String): String = uri
}

/**
 * This class encapsulates a string used as the value of the document-uri() property of a document,
 * together with a normalized representation of the string used for equality comparisons. The idea
 * is that on Windows systems, document URIs are compared using case-blind comparison, but the original
 * case is retained for display purposes.
 */
class DocumentURI(uri: String) {

  private val displayValue: String = uri

  private val normalizedValue: String = normalizeURI(uri)

  if (uri == null) {
    throw new NullPointerException("uri")
  }

  override def toString(): String = displayValue

  override def equals(obj: Any): Boolean = obj match {
    case obj: DocumentURI ⇒ normalizedValue == obj.normalizedValue
    case _ ⇒ false
  }

  override def hashCode(): Int = normalizedValue.hashCode
}
