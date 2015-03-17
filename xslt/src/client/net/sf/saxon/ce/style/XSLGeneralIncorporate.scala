// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.functions.DocumentFn
import client.net.sf.saxon.ce.om.DocumentInfo
import client.net.sf.saxon.ce.om.DocumentURI
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.linked.DocumentImpl
import client.net.sf.saxon.ce.tree.linked.ElementImpl
import client.net.sf.saxon.ce.tree.util.URI
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Class to represent xsl:include or xsl:import element in the stylesheet. <br>
 * The xsl:include and xsl:import elements have mandatory attribute href
 */
class XSLGeneralIncorporate extends StyleElement {

  private var href: String = _

  /**
   * Ask whether this node is a declaration, that is, a permitted child of xsl:stylesheet
   * (including xsl:include and xsl:import).
   * @return true for this element
   */
  override def isDeclaration(): Boolean = true

  def prepareAttributes() {
    href = checkAttribute("href", "w1").asInstanceOf[String]
    checkForUnknownAttributes()
  }

  def validate(decl: Declaration) {
    validateInstruction()
  }

  def validateInstruction() {
    checkEmpty()
    checkTopLevel(if (getLocalPart == "import") "XTSE0190" else "XTSE0170")
  }

  /**
   * Get the included or imported stylesheet module
   * @param importer the module that requested the include or import (used to check for cycles)
   * @param precedence the import precedence to be allocated to the included or imported module
   * @return the xsl:stylesheet element at the root of the included/imported module
   * @throws XPathException if any failure occurs
   */
  def getIncludedStylesheet(importer: StylesheetModule, precedence: Int): StylesheetModule = {
    if (href == null) {
      return null
    }
    try {
      val psm = importer.getPrincipalStylesheetModule
      val pss = psm.getExecutable
      var includedSheet: XSLStylesheet = null
      var incModule: StylesheetModule = null
      val key = DocumentFn.computeDocumentKey(href, getBaseURI)
      includedSheet = psm.getStylesheetDocument(key)
      if (includedSheet != null) {
        incModule = new StylesheetModule(includedSheet, precedence)
        incModule.setImporter(importer)
      } else {
        var relative = href
        var fragment: String = null
        val hash = relative.indexOf('#')
        if (hash == 0 || relative.length == 0) {
          reportCycle()
          return null
        } else if (hash == relative.length - 1) {
          relative = relative.substring(0, hash)
        } else if (hash > 0) {
          if (hash + 1 < relative.length) {
            fragment = relative.substring(hash + 1)
          }
          relative = relative.substring(0, hash)
        }
        var source: String = null
        val base = new URI(getBaseURI)
        val abs = base.resolve(relative)
        source = abs.toString
        var anc = importer
        if (source != null) {
          while (anc != null) {
            if (source == anc.getSourceElement.getSystemId) {
              reportCycle()
              return null
            }
            anc = anc.getImporter
          }
        }
        val rawDoc = getConfiguration.buildDocument(source)
        getConfiguration.getDocumentPool.add(rawDoc, key)
        var includedDoc = pss.loadStylesheetModule(rawDoc)
        var outermost = includedDoc.getDocumentElement
        if (outermost.isInstanceOf[LiteralResultElement]) {
          includedDoc = outermost.asInstanceOf[LiteralResultElement].makeStylesheet(getPreparedStylesheet)
          outermost = includedDoc.getDocumentElement
        }
        if (!(outermost.isInstanceOf[XSLStylesheet])) {
          compileError("Included document " + href + " is not a stylesheet", "XTSE0165")
          return null
        }
        includedSheet = outermost.asInstanceOf[XSLStylesheet]
        includedSheet.setPrincipalStylesheetModule(psm)
        psm.putStylesheetDocument(key, includedSheet)
        incModule = new StylesheetModule(includedSheet, precedence)
        incModule.setImporter(importer)
        val decl = new Declaration(incModule, includedSheet)
        includedSheet.validate(decl)
        if (includedSheet.validationError != null) {
          if (reportingCircumstances == REPORT_ALWAYS) {
            includedSheet.compileError(includedSheet.validationError)
          } else if (includedSheet.reportingCircumstances == REPORT_UNLESS_FORWARDS_COMPATIBLE) {
            includedSheet.compileError(includedSheet.validationError)
          }
        }
      }
      incModule.spliceIncludes()
      importer.setInputTypeAnnotations(includedSheet.getInputTypeAnnotationsAttribute | incModule.getInputTypeAnnotations)
      incModule
    } catch {
      case err: XPathException => {
        err.setErrorCode("XTSE0165")
        err.setIsStaticError(true)
        compileError(err)
        null
      }
    }
  }

  private def reportCycle() {
    compileError("A stylesheet cannot " + getLocalPart + " itself", (if (getLocalPart == "include") "XTSE0180" else "XTSE0210"))
  }

  def compile(exec: Executable, decl: Declaration): Expression = null
}
