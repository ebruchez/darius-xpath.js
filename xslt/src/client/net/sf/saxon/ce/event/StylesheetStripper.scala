// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.event

import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.Whitespace
import java.util.Arrays
import StylesheetStripper._
//remove if not needed
import scala.collection.JavaConversions._

object StylesheetStripper {

  val ALWAYS_PRESERVE = 0x01

  val ALWAYS_STRIP = 0x02

  val STRIP_DEFAULT = 0x00

  val PRESERVE_PARENT = 0x04

  val CANNOT_STRIP = 0x08

  private val specials = Array("analyze-string", "apply-imports", "apply-templates", "attribute-set", "call-template", "character-map", "choose", "stylesheet", "transform")

  private val XML_SPACE = new StructuredQName("xml", NamespaceConstant.XML, "space")
}

/**
 * The Stripper class performs whitespace stripping according to the rules of
 * the xsl:strip-space and xsl:preserve-space instructions.
 * It maintains details of which elements need to be stripped.
 * The code is written to act as a SAX-like filter to do the stripping.
 * @author Michael H. Kay
 */
class StylesheetStripper extends ProxyReceiver {

  private var stripStack: Array[Byte] = new Array[Byte](100)

  private var top: Int = 0

  /**
   * Decide whether an element is in the set of white-space preserving element types
   * @param elementName identifies the element being tested
   */
  def isSpacePreserving(elementName: StructuredQName): Byte = {
    if (elementName.getNamespaceURI == NamespaceConstant.XSLT) {
      val local = elementName.getLocalName
      if (local == "text") {
        return ALWAYS_PRESERVE
      }
      if (Arrays.binarySearch(specials, local) >= 0) {
        return ALWAYS_STRIP
      }
    }
    STRIP_DEFAULT
  }

  /**
   * Callback interface for SAX: not for application use
   */
  def open() {
    top = 0
    stripStack(top) = ALWAYS_PRESERVE
    super.open()
  }

  def startElement(qName: StructuredQName, properties: Int) {
    nextReceiver.startElement(qName, properties)
    val preserveParent = stripStack(top)
    var preserve = (preserveParent & PRESERVE_PARENT).toByte
    val elementStrip = isSpacePreserving(qName)
    if (elementStrip == ALWAYS_PRESERVE) {
      preserve |= ALWAYS_PRESERVE
    } else if (elementStrip == ALWAYS_STRIP) {
      preserve |= ALWAYS_STRIP
    }
    top += 1
    if (top >= stripStack.length) {
      val newStack = Array.ofDim[Byte](top * 2)
      System.arraycopy(stripStack, 0, newStack, 0, top)
      stripStack = newStack
    }
    stripStack(top) = preserve
  }

  def attribute(nameCode: StructuredQName, value: CharSequence) {
    if (nameCode == XML_SPACE) {
      if (value.toString == "preserve") {
        stripStack(top) |= PRESERVE_PARENT
      } else {
        stripStack(top) &= ~PRESERVE_PARENT
      }
    }
    nextReceiver.attribute(nameCode, value)
  }

  /**
   * Handle an end-of-element event
   */
  def endElement() {
    nextReceiver.endElement()
    top -= 1
  }

  /**
   * Handle a text node
   */
  def characters(chars: CharSequence) {
    if (((((stripStack(top) & (ALWAYS_PRESERVE | PRESERVE_PARENT | CANNOT_STRIP)) != 
      0) && 
      (stripStack(top) & ALWAYS_STRIP) == 0) || 
      !Whitespace.isWhite(chars)) && 
      chars.length > 0) {
      nextReceiver.characters(chars)
    }
  }
}
