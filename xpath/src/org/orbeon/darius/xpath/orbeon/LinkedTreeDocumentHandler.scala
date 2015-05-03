/**
 * Copyright 2015 Orbeon, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.orbeon.darius.xpath.orbeon

import org.orbeon.darius.xpath.event.PipelineConfiguration
import org.orbeon.darius.xpath.om.NamespaceBinding
import org.orbeon.darius.xpath.om.StructuredQName
import org.orbeon.darius.xpath.tree.linked.LinkedTreeBuilder
import org.orbeon.darius.util.XMLSymbols
import org.orbeon.darius.xni.Augmentations
import org.orbeon.darius.xni.NamespaceContext
import org.orbeon.darius.xni.QName
import org.orbeon.darius.xni.XMLAttributes
import org.orbeon.darius.xni.XMLDocumentHandler
import org.orbeon.darius.xni.XMLLocator
import org.orbeon.darius.xni.XMLResourceIdentifier
import org.orbeon.darius.xni.XMLString
import org.orbeon.darius.xni.parser.XMLDocumentSource

// Build a LinkedTree from Darius XMLContentHandler events
class LinkedTreeDocumentHandler(config: PipelineConfiguration) extends XMLDocumentHandler {
  
  private val builder = new LinkedTreeBuilder
  private var source: Option[XMLDocumentSource] = None
  private var namespaceContext: NamespaceContext = null
  
  builder.setPipelineConfiguration(config)
  
  def setDocumentSource(source: XMLDocumentSource): Unit = this.source = Option(source)
  def getDocumentSource: XMLDocumentSource = source.orNull
  
  def result = builder.getCurrentRoot
  
  def startDocument(locator: XMLLocator, encoding: String, namespaceContext: NamespaceContext, augs: Augmentations): Unit = {
    
    this.namespaceContext = namespaceContext
    
    builder.open()
    builder.startDocument()
  }
  
  def endDocument(augs: Augmentations): Unit = {
    builder.endDocument()
    builder.close()
  }
  
  def startElement(element: QName, attributes: XMLAttributes, augs: Augmentations): Unit = {
    
    builder.startElement(
      new StructuredQName(
        prefix    = Option(element.prefix) getOrElse "",
        uri       = element.uri,
        localName = element.localpart
      ),
      0
    )
    
    for {
      index  ← 0 until namespaceContext.getDeclaredPrefixCount
      prefix = namespaceContext.getDeclaredPrefixAt(index)
      uri   = namespaceContext.getURI(prefix)
    } locally {
      builder.namespace(NamespaceBinding(prefix, uri), 0)
    }
    
    for {
      index     ← 0 until attributes.getLength
      prefix    = attributes.getPrefix(index)
      uri       = attributes.getURI(index)
      localpart = attributes.getLocalName(index)
      rawname   = attributes.getQName(index)
      if prefix != XMLSymbols.PREFIX_XMLNS && rawname != XMLSymbols.PREFIX_XMLNS
    } locally {
      builder.attribute(
        new StructuredQName(
          prefix    = Option(prefix) getOrElse "",
          uri       = uri,
          localName = localpart
        ),
        attributes.getValue(index)
      )
    }
    
    builder.startContent()
  }
  
  def endElement(element: QName, augs: Augmentations): Unit = {
    builder.endElement()
  }
  
  def emptyElement(element: QName, attributes: XMLAttributes, augs: Augmentations): Unit = {
    startElement(element, attributes, augs)
    endElement(element, augs)
  }
  
  def characters(text: XMLString, augs: Augmentations): Unit = {
    // NOTE: Doc says "adjacent text nodes must have already been merged" but LinkedTreeBuilder handles merging
    builder.characters(text.toString())
  }

  def comment(text: XMLString, augs: Augmentations): Unit = {
    // @ebruchez: could we make XMLString a CharSequence?
    builder.comment(text.toString())
  }
  
  def processingInstruction(target: String, data: XMLString, augs: Augmentations): Unit = {
    // @ebruchez check this
    builder.processingInstruction(target, data.toString())
  }

  // Events we don't care about
  def xmlDecl(version: String, encoding: String, standalone: String, augs: Augmentations) = ()
  def doctypeDecl(rootElement: String, publicId: String, systemId: String, augs: Augmentations) = ()
  def textDecl(version: String, encoding: String, augs: Augmentations) = ()
  def startGeneralEntity(name: String, identifier: XMLResourceIdentifier, encoding: String, augs: Augmentations) = ()
  def endGeneralEntity(name: String, augs: Augmentations) = ()
  def ignorableWhitespace(text: XMLString, augs: Augmentations) = ()
  def startCDATA(augs: Augmentations) = ()
  def endCDATA(augs: Augmentations) = ()
}
