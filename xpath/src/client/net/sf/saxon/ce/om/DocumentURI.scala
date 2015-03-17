package client.net.sf.saxon.ce.om

import DocumentURI._
//remove if not needed
import scala.collection.JavaConversions._

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

  private var displayValue: String = uri

  private var normalizedValue: String = normalizeURI(uri)

  if (uri == null) {
    throw new NullPointerException("uri")
  }

  override def toString(): String = displayValue

  override def equals(obj: Any): Boolean = obj match {
    case obj: DocumentURI => normalizedValue == obj.normalizedValue
    case _ => false
  }

  override def hashCode(): Int = normalizedValue.hashCode
}
