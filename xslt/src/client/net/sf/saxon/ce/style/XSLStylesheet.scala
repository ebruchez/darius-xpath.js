// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.trans.RuleManager
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.linked.NodeImpl
import client.net.sf.saxon.ce.`type`.Type
import XSLStylesheet._
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

object XSLStylesheet {

  val ANNOTATION_UNSPECIFIED = 0

  val ANNOTATION_STRIP = 1

  val ANNOTATION_PRESERVE = 2
}

/**
 * An xsl:stylesheet or xsl:transform element in the stylesheet. <br>
 * Note this element represents a stylesheet module, not necessarily
 * the whole stylesheet. However, much of the functionality (and the fields)
 * are relevant only to the top-level module.
 */
class XSLStylesheet extends StyleElement {

  var exec: Executable = _

  @BeanProperty
  var principalStylesheetModule: PrincipalStylesheetModule = _

  @BeanProperty
  var defaultMode: StructuredQName = null

  /**
   * Get the owning PreparedStylesheet object.
   * @return the owning PreparedStylesheet object. Exceptionally returns null during early construction.
   */
  def getPreparedStylesheet(): Executable = {
    if (principalStylesheetModule == null) null else principalStylesheetModule.getExecutable
  }

  def setPrincipalStylesheetModule(module: PrincipalStylesheetModule): Unit = {
    this.principalStylesheetModule = module
    this.exec = module.getExecutable
  }

  /**
   * Get the run-time Executable object
   */
  def getExecutable(): Executable = exec

  protected def mayContainParam(attName: String): Boolean = true

  /**
   * Get the RuleManager which handles template rules
   * @return the template rule manager
   */
  def getRuleManager(): RuleManager = exec.getRuleManager

  /**
   * Prepare the attributes on the stylesheet element
   */
  def prepareAttributes(): Unit = {
    checkAttribute("version", "s1")
    checkAttribute("id", "s")
    checkAttribute("extension-element-prefixes", "s")
    checkAttribute("exclude-result-prefixes", "s")
    checkAttribute("default-validation", "v")
    val inputTypeAnnotationsAtt = checkAttribute("input-type-annotations", "w").asInstanceOf[String]
    checkForUnknownAttributes()
    if (inputTypeAnnotationsAtt != null) {
      if (inputTypeAnnotationsAtt == "strip") {
      } else if (inputTypeAnnotationsAtt == "preserve") {
      } else if (inputTypeAnnotationsAtt == "unspecified") {
      } else {
        compileError("Invalid value for input-type-annotations attribute. " + 
          "Permitted values are (strip, preserve, unspecified)", "XTSE0020")
      }
    }
  }

  /**
   * Get the value of the input-type-annotations attribute, for this module alone.
   * The value is an or-ed combination of the two bits
   * [[#ANNOTATION_STRIP]] and [[#ANNOTATION_PRESERVE]]
   * @return the value if the input-type-annotations attribute in this stylesheet module
   */
  def getInputTypeAnnotationsAttribute(): Int = {
    val inputTypeAnnotationsAtt = getAttributeValue("", "input-type-annotations")
    if (inputTypeAnnotationsAtt != null) {
      if (inputTypeAnnotationsAtt == "strip") {
        return ANNOTATION_STRIP
      } else if (inputTypeAnnotationsAtt == "preserve") {
        return ANNOTATION_PRESERVE
      } else if (inputTypeAnnotationsAtt == "unspecified") {
        return ANNOTATION_UNSPECIFIED
      } else {
        compileError("Invalid value for input-type-annotations attribute. " + 
          "Permitted values are (strip, preserve, unspecified)", "XTSE0020")
      }
    }
    -1
  }

  /**
   * Validate this element
   * @param decl
   */
  def validate(decl: Declaration): Unit = {
    if (validationError != null) {
      compileError(validationError)
    }
    if (getParent.getNodeKind != Type.DOCUMENT) {
      compileError(getDisplayName + " must be the outermost element", "XTSE0010")
    }
    for (child <- allChildren()) {
      if (child.getNodeKind == Type.TEXT || 
        (child.isInstanceOf[StyleElement] && child.asInstanceOf[StyleElement].isDeclaration) || 
        child.isInstanceOf[DataElement]) {
      } else if (NamespaceConstant.XSLT != child.getURI && "" != child.getURI) {
      } else if (child.isInstanceOf[AbsentExtensionElement] && 
        child.asInstanceOf[StyleElement].forwardsCompatibleModeIsEnabled()) {
      } else if (NamespaceConstant.XSLT == child.getURI) {
        child.asInstanceOf[StyleElement].compileError("Element " + child.getDisplayName + " must not appear directly within " + 
          getDisplayName, "XTSE0010")
      } else {
        child.asInstanceOf[StyleElement].compileError("Element " + child.getDisplayName + " must not appear directly within " + 
          getDisplayName + 
          " because it is not in a namespace", "XTSE0130")
      }
    }
  }

  /**
   * Process the attributes of every node in the stylesheet
   */
  def processAllAttributes(): Unit = {
    processDefaultCollationAttribute("")
    prepareAttributes()
    for (child <- allChildren() if child.isInstanceOf[StyleElement]) {
      try {
        child.asInstanceOf[StyleElement].processAllAttributes()
      } catch {
        case err: XPathException => child.asInstanceOf[StyleElement].compileError(err)
      }
    }
  }

  /**
   * Dummy compile() method to satisfy the interface
   */
  def compile(exec: Executable, decl: Declaration): Expression = null
}
