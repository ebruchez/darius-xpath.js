// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.event.PipelineConfiguration
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om._
import client.net.sf.saxon.ce.orbeon.Configuration
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.linked.ElementImpl
import client.net.sf.saxon.ce.tree.linked.NodeFactory
import client.net.sf.saxon.ce.value.DecimalValue
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Class StyleNodeFactory. <br>
 * A Factory for nodes in the stylesheet tree. <br>
 * Currently only allows Element nodes to be user-constructed.
 *
 * @author Michael H. Kay
 */
class StyleNodeFactory(protected var config: Configuration) extends NodeFactory {

  /**
   * Create an Element node. Note, if there is an error detected while constructing
   * the Element, we add the element anyway, and return success, but flag the element
   * with a validation error. This allows us to report more than
   * one error from a single compilation.
   *
   * @param nameCode The element name
   * @param attlist  the attribute list
   */
  def makeElementNode(parent: NodeInfo, 
      nameCode: StructuredQName, 
      attlist: AttributeCollection, 
      namespaces: Array[NamespaceBinding], 
      namespacesUsed: Int, 
      pipe: PipelineConfiguration, 
      baseURI: String, 
      sequence: Int): ElementImpl = {
    val toplevel = parent.isInstanceOf[XSLStylesheet]
    if (parent.isInstanceOf[DataElement]) {
      val d = new DataElement()
      d.setNamespaceDeclarations(namespaces, namespacesUsed)
      d.initialise(nameCode, attlist, parent, sequence)
      d.setLocation(baseURI)
      return d
    }
    var e: StyleElement = null
    if (nameCode.getNamespaceURI == NamespaceConstant.XSLT) {
      e = makeXSLElement(nameCode.getLocalName)
    }
    if (e != null) {
      e.setNamespaceDeclarations(namespaces, namespacesUsed)
      e.initialise(nameCode, attlist, parent, sequence)
      e.setLocation(baseURI)
      try {
        e.processExtensionElementAttribute("")
      } catch {
        case err: XPathException => e.setValidationError(err, StyleElement.REPORT_ALWAYS)
      }
      try {
        e.processExcludedNamespaces("")
      } catch {
        case err: XPathException => e.setValidationError(err, StyleElement.REPORT_ALWAYS)
      }
      try {
        e.processVersionAttribute("")
      } catch {
        case err: XPathException => e.setValidationError(err, StyleElement.REPORT_ALWAYS)
      }
      e.processDefaultXPathNamespaceAttribute("")
      return e
    }
    val uriCode = nameCode.getNamespaceURI
    if (parent.isInstanceOf[XSLStylesheet] && !uriCode.isEmpty && 
      uriCode != NamespaceConstant.XSLT) {
      val d = new DataElement()
      d.setNamespaceDeclarations(namespaces, namespacesUsed)
      d.initialise(nameCode, attlist, parent, sequence)
      d.setLocation(baseURI)
      d
    } else {
      val localname = nameCode.getLocalName
      var temp: StyleElement = null
      if (uriCode == NamespaceConstant.XSLT && parent.isInstanceOf[XSLStylesheet] &&
        parent.asInstanceOf[XSLStylesheet].getEffectiveVersion
        .compareTo(DecimalValue.TWO) <= 
        0) {
        temp = new AbsentExtensionElement()
        temp.setValidationError(new XPathException("Unknown top-level XSLT declaration"), StyleElement.REPORT_UNLESS_FORWARDS_COMPATIBLE)
      }
      val assumedElement = new LiteralResultElement()
      if (temp == null) {
        temp = new LiteralResultElement()
      }
      temp.setNamespaceDeclarations(namespaces, namespacesUsed)
      try {
        temp.initialise(nameCode, attlist, parent, sequence)
        temp.setLocation(baseURI)
        temp.processStandardAttributes(NamespaceConstant.XSLT)
      } catch {
        case err: XPathException => temp.setValidationError(err, StyleElement.REPORT_ALWAYS)
      }
      var reason: XPathException = null
      var actualElement: StyleElement = null
      if (uriCode == NamespaceConstant.XSLT) {
        reason = new XPathException("Unknown XSLT element: " + localname)
        reason.setErrorCode("XTSE0010")
        reason.setIsStaticError(true)
        actualElement = new AbsentExtensionElement()
        temp.setValidationError(reason, StyleElement.REPORT_UNLESS_FALLBACK_AVAILABLE)
      } else if (temp.isExtensionNamespace(uriCode) && !toplevel) {
        val uri = nameCode.getNamespaceURI
        if (NamespaceConstant.IXSL == uri) {
          actualElement = makeIXSLElement(localname)
        }
        if (actualElement == null) {
          actualElement = new AbsentExtensionElement()
          val se = new XPathException("Unknown extension instruction", temp)
          se.setErrorCode("XTDE1450")
          reason = se
          temp.setValidationError(reason, StyleElement.REPORT_IF_INSTANTIATED)
        }
      } else {
        actualElement = new LiteralResultElement()
      }
      var node: StyleElement = null
      if (actualElement.getClass == assumedElement.getClass) {
        node = temp
      } else {
        node = actualElement
        node.substituteFor(temp)
      }
      node
    }
  }

  /**
   * Make an element in the IXSL namespace
   * @param localName the local name of the instruction
   * @return the constructed element node
   */
  def makeIXSLElement(localName: String): StyleElement = {
    if (localName == "set-attribute") {
      new IXSLSetAttribute()
    } else if (localName == "remove-attribute") {
      new IXSLRemoveAttribute()
    } else if (localName == "schedule-action") {
      new IXSLScheduleAction()
    } else if (localName == "set-property") {
      new IXSLSetProperty()
    } else {
      null
    }
  }

  /**
   * Make an XSL element node
   *
   * @param localName the node name
   * @return the constructed element node
   */
  def makeXSLElement(localName: String): StyleElement = localName.charAt(0) match {
    case 'a' => if (localName == "analyze-string") {
      new XSLAnalyzeString()
    } else if (localName == "apply-imports") {
      new XSLApplyImports()
    } else if (localName == "apply-templates") {
      new XSLApplyTemplates()
    } else if (localName == "attribute") {
      new XSLAttribute()
    } else if (localName == "attribute-set") {
      new XSLAttributeSet()
    }
    case 'c' => if (localName == "call-template") {
      new XSLCallTemplate()
    } else if (localName == "character-map") {
      new XSLCharacterMap()
    } else if (localName == "choose") {
      new XSLChoose()
    } else if (localName == "comment") {
      new XSLComment()
    } else if (localName == "copy") {
      new XSLCopy()
    } else if (localName == "copy-of") {
      new XSLCopyOf()
    }
    case 'd' => if (localName == "decimal-format") {
      new XSLDecimalFormat()
    } else if (localName == "document") {
      new XSLDocument()
    }
    case 'e' => if (localName == "element") {
      new XSLElement()
    }
    case 'f' => if (localName == "fallback") {
      new XSLFallback()
    } else if (localName == "for-each") {
      new XSLForEach()
    } else if (localName == "for-each-group") {
      new XSLForEachGroup()
    } else if (localName == "function") {
      new XSLFunction()
    }
    case 'i' => if (localName == "if") {
      new XSLIf()
    } else if (localName == "import") {
      new XSLGeneralIncorporate()
    } else if (localName == "import-schema") {
      new XSLImportSchema()
    } else if (localName == "include") {
      new XSLGeneralIncorporate() {
      }
    }
    case 'k' => if (localName == "key") {
      new XSLKey()
    }
    case 'm' => if (localName == "matching-substring") {
      new XSLMatchingSubstring()
    } else if (localName == "message") {
      new XSLMessage()
    }
    case 'n' => if (localName == "next-match") {
      new XSLNextMatch()
    } else if (localName == "non-matching-substring") {
      new XSLMatchingSubstring()
    } else if (localName == "number") {
      new XSLNumber()
    } else if (localName == "namespace") {
      new XSLMinorNodeConstructor()
    } else if (localName == "namespace-alias") {
      new XSLNamespaceAlias()
    }
    case 'o' => if (localName == "otherwise") {
      new XSLOtherwise()
    } else if (localName == "output") {
      new XSLOutput()
    } else if (localName == "output-character") {
      new XSLOutputCharacter()
    }
    case 'p' => if (localName == "param") {
      new XSLParam()
    } else if (localName == "perform-sort") {
      new XSLPerformSort()
    } else if (localName == "preserve-space") {
      new XSLPreserveSpace()
    } else if (localName == "processing-instruction") {
      new XSLMinorNodeConstructor()
    }
    case 'r' => if (localName == "result-document") {
      new XSLResultDocument()
    }
    case 's' => if (localName == "sequence") {
      new XSLSequence()
    } else if (localName == "sort") {
      new XSLSort()
    } else if (localName == "strip-space") {
      new XSLPreserveSpace()
    } else if (localName == "stylesheet") {
      new XSLStylesheet()
    }
    case 't' => if (localName == "template") {
      new XSLTemplate()
    } else if (localName == "text") {
      new XSLText()
    } else if (localName == "transform") {
      new XSLStylesheet()
    }
    case 'v' => if (localName == "value-of") {
      new XSLValueOf()
    } else if (localName == "variable") {
      new XSLVariable()
    }
    case 'w' => if (localName == "with-param") {
      new XSLWithParam()
    } else if (localName == "when") {
      new XSLWhen()
    }
    case _ => null
  }

  /**
   * Method to support the element-available() function
   *
   * @param uri       the namespace URI
   * @param localName the local Name
   * @return true if an extension element of this name is recognized
   */
  def isElementAvailable(uri: String, localName: String): Boolean = {
    if (uri == NamespaceConstant.XSLT) {
      val e = makeXSLElement(localName)
      if (e != null) {
        return e.isInstruction
      }
    }
    false
  }
}
