// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.linked.NodeImpl
import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.value.Whitespace
import java.util.ArrayList
import java.util.List
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A stylesheet module represents a module of a stylesheet. It is possible for two modules
 * to share the same stylesheet tree in the case where two includes or imports reference
 * the same URI; in this case the two modules will typically have a different import precedence.
 */
class StylesheetModule(@BeanProperty var sourceElement: XSLStylesheet, var precedence: Int)
    {

  @BeanProperty
  var minImportPrecedence: Int = _

  @BeanProperty
  var importer: StylesheetModule = _

  var wasIncluded: Boolean = _

  @BeanProperty
  var inputTypeAnnotations: Int = 0

  protected var topLevel: List[Declaration] = new ArrayList[Declaration]()

  def getPrincipalStylesheetModule(): PrincipalStylesheetModule = importer.getPrincipalStylesheetModule

  def getPrecedence(): Int = {
    if (wasIncluded) importer.getPrecedence else precedence
  }

  /**
   * Indicate that this stylesheet was included (by its "importer") using an xsl:include
   * statement as distinct from xsl:import
   */
  def setWasIncluded(): Unit = {
    wasIncluded = true
  }

  /**
   * Process xsl:include and xsl:import elements.
   */
  def spliceIncludes(): Unit = {
    var foundNonImport = false
    topLevel = new ArrayList[Declaration](50)
    minImportPrecedence = precedence
    var previousElement = sourceElement
    for (child ← sourceElement.allChildren()) {
      if (child.getNodeKind == Type.TEXT) {
        if (!Whitespace.isWhite(child.getStringValue)) {
          previousElement.compileError("No character data is allowed between top-level elements", "XTSE0120")
        }
      } else if (child.isInstanceOf[DataElement]) {
        foundNonImport = true
      } else {
        previousElement = child.asInstanceOf[StyleElement]
        if (child.isInstanceOf[XSLGeneralIncorporate]) {
          val xslinc = child.asInstanceOf[XSLGeneralIncorporate]
          xslinc.processAttributes()
          if ("import" == xslinc.getLocalPart) {
            if (foundNonImport) {
              xslinc.compileError("xsl:import elements must come first", "XTSE0200")
            }
          } else {
            foundNonImport = true
          }
          xslinc.validateInstruction()
          var errors = sourceElement.getPreparedStylesheet.getErrorCount
          val inc = xslinc.getIncludedStylesheet(this, precedence)
          if (inc == null) {
            return
          }
          errors = sourceElement.getPreparedStylesheet.getErrorCount - errors
          if (errors > 0) {
            xslinc.compileError("Reported " + errors + (if (errors == 1) " error" else " errors") + 
              " in " + 
              xslinc.getLocalPart.substring(0, 6) + 
              "ed stylesheet module", "XTSE0165")
          }
          if (xslinc.getLocalPart == "import") {
            precedence = inc.getPrecedence + 1
          } else {
            precedence = inc.getPrecedence
            inc.setMinImportPrecedence(minImportPrecedence)
            inc.setWasIncluded()
          }
          val incchildren = inc.topLevel
          for (j ← 0 until incchildren.size) {
            val decl = incchildren.get(j)
            var last = topLevel.size - 1
            if (last < 0 || 
              decl.getPrecedence >= topLevel.get(last).getPrecedence) {
              topLevel.add(decl)
            } else {
              while (last >= 0 && 
                decl.getPrecedence < topLevel.get(last).getPrecedence) {
                last -= 1
              }
              topLevel.add(last + 1, decl)
            }
          }
        } else {
          foundNonImport = true
          val decl = new Declaration(this, child.asInstanceOf[StyleElement])
          topLevel.add(decl)
        }
      }
    }
  }

  /**
   * Set the value of the input-type-annotations attribute, for this module combined with that
   * of all included/imported modules. The value is an or-ed combination of the two bits
   * [[XSLStylesheet#ANNOTATION_STRIP]] and [[XSLStylesheet#ANNOTATION_PRESERVE]]
   * @param annotations the value of the input-type-annotations attribute, for this module combined with that
   * of all included/imported modules.
   */
  def setInputTypeAnnotations(annotations: Int): Unit = {
    inputTypeAnnotations |= annotations
    if (inputTypeAnnotations == 
      (XSLStylesheet.ANNOTATION_STRIP | XSLStylesheet.ANNOTATION_PRESERVE)) {
      getPrincipalStylesheetModule.compileError("One stylesheet module specifies input-type-annotations='strip', " + 
        "another specifies input-type-annotations='preserve'", "XTSE0265")
    }
  }
}
