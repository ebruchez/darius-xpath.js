// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.om

import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om.StructuredQName._
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.util.FastStringBuffer
import client.net.sf.saxon.ce.value.Whitespace

object StructuredQName {

  private val EMPTY_STRING = ""

  /**
   * Make a structuredQName from a Clark name
   * @param expandedName the name in Clark notation "{uri}local" if in a namespace, or "local" otherwise.
   * The format "{}local" is also accepted for a name in no namespace.
   * @return the constructed StructuredQName
   * @throws IllegalArgumentException if the Clark name is malformed
   */
  def fromClarkName(expandedName: String): StructuredQName = {
    var namespace: String = null
    var localName: String = null
    if (expandedName.charAt(0) == '{') {
      val closeBrace = expandedName.indexOf('}')
      if (closeBrace < 0) {
        throw new IllegalArgumentException("No closing '}' in Clark name")
      }
      namespace = expandedName.substring(1, closeBrace)
      if (closeBrace == expandedName.length) {
        throw new IllegalArgumentException("Missing local part in Clark name")
      }
      localName = expandedName.substring(closeBrace + 1)
    } else {
      namespace = ""
      localName = expandedName
    }
    new StructuredQName("", namespace, localName)
  }

  /**
   * Make a structured QName from a lexical QName, using a supplied NamespaceResolver to
   * resolve the prefix
   * @param lexicalName the QName as a lexical name (prefix:local)
   * @param defaultURI the uri to be used if there is no prefix
   * @param resolver NamespaceResolver used to look up a URI for the prefix
   * @return the StructuredQName object corresponding to this lexical QName
   * @throws XPathException if the namespace prefix is not in scope or if the value is lexically
   * invalid. Error code FONS0004 is set if the namespace prefix has not been declared; error
   * code FOCA0002 is set if the name is lexically invalid.
   */
  def fromLexicalQName(lexicalName: CharSequence, defaultURI: String, resolver: NamespaceResolver): StructuredQName = {
    try {
      val parts = NameChecker.getQNameParts(Whitespace.trimWhitespace(lexicalName))
      val prefix = parts(0)
      val uri = (if (prefix == "") defaultURI else resolver.getURIForPrefix(prefix, false))
      if (uri == null) {
        val de = new XPathException("Namespace prefix '" + parts(0) + "' has not been declared")
        de.setErrorCode("FONS0004")
        throw de
      }
      new StructuredQName(parts(0), uri, parts(1))
    } catch {
      case e: QNameException => {
        val de = new XPathException(e.getMessage)
        de.setErrorCode("FOCA0002")
        throw de
      }
    }
  }

  val XML_ID = new StructuredQName("xml", NamespaceConstant.XML, "id")
}

/**
 * This class provides an economical representation of a QName triple (prefix, URI, and localname).
 * The value is stored internally as a character array containing the concatenation of URI, localname,
 * and prefix (in that order) with two integers giving the start positions of the localname and prefix.
 *
 * <p><i>Instances of this class are immutable.</i></p>
 */
class StructuredQName(prefix: String, var uri: String, localName: String) extends Comparable[StructuredQName] {

  if (uri == null) {
    uri = ""
  }

  val plen = prefix.length
  val ulen = uri.length
  val llen = localName.length

  private val content = new Array[Char](ulen + llen + plen)
  private var localNameStart = ulen
  private var prefixStart = ulen + llen

  uri.getChars(0, ulen, content, 0)
  localName.getChars(0, llen, content, ulen)
  prefix.getChars(0, plen, content, ulen + llen)

  /**
   * Get the prefix of the QName.
   * @return the prefix. Returns the empty string if the name is unprefixed.
   */
  def getPrefix(): String = {
    new String(content, prefixStart, content.length - prefixStart)
  }

  /**
   * Get the namespace URI of the QName.
   * @return the URI. Returns the empty string to represent the no-namespace
   */
  def getNamespaceURI(): String = {
    if (localNameStart == 0) {
      return EMPTY_STRING
    }
    new String(content, 0, localNameStart)
  }

  /**
   * Get the local part of the QName
   * @return the local part of the QName
   */
  def getLocalName(): String = {
    new String(content, localNameStart, prefixStart - localNameStart)
  }

  /**
   * Get the display name, that is the lexical QName in the form [prefix:]local-part
   * @return the lexical QName
   */
  def getDisplayName(): String = {
    if (prefixStart == content.length) {
      getLocalName
    } else {
      val buff = new FastStringBuffer(content.length - localNameStart + 1)
      buff.append(content, prefixStart, content.length - prefixStart)
      buff.append(':')
      buff.append(content, localNameStart, prefixStart - localNameStart)
      buff.toString
    }
  }

  /**
   * Get the expanded QName in Clark format, that is "{uri}local" if it is in a namespace, or just "local"
   * otherwise.
   * @return the QName in Clark notation
   */
  def getClarkName(): String = {
    val buff = new FastStringBuffer(content.length - prefixStart + 2)
    if (localNameStart > 0) {
      buff.append('{')
      buff.append(content, 0, localNameStart)
      buff.append('}')
    }
    buff.append(content, localNameStart, prefixStart - localNameStart)
    buff.toString
  }

  /**
   * The toString() method displays the QName as a lexical QName, that is prefix:local
   * @return the lexical QName
   */
  override def toString(): String = getDisplayName

  /**
   * Compare two StructuredQName values for equality. This compares the URI and local name parts,
   * excluding any prefix
   */
  override def equals(other: Any): Boolean = other match {
    case other: StructuredQName => {
      val sq2 = other
      if (localNameStart != sq2.localNameStart || prefixStart != sq2.prefixStart) {
        return false
      }
      var i = prefixStart - 1
      while (i >= 0) {
        if (content(i) != sq2.content(i)) {
          return false
        }
        i -= 1
        i -= 1
      }
      true
    }
    case _ => false
  }

  /**
   * Get a hashcode to reflect the equals() method
   * @return a hashcode based on the URI and local part only, ignoring the prefix.
   */
  override def hashCode(): Int = {
    var h = 0x8004a00b
    h ^= prefixStart
    h ^= localNameStart
    var i = prefixStart - 1
    while (i >= 0) {
      h ^= (content(i) << (i & 0x1f))
      i -= 1
    }
    h
  }

  /**
   * Compare QNames alphabetically. Used to establish document order for attribute nodes
   */
  def compareTo(other: StructuredQName): Int = {
    (0 until prefixStart).find(i => content(i) != other.content(i))
      .map(i => if (content(i) < other.content(i)) -1 else +1)
      .getOrElse(0)
  }
}
