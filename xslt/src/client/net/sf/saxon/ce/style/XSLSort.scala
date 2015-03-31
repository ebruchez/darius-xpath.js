// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.expr.sort.CodepointCollator
import client.net.sf.saxon.ce.expr.sort.SortKeyDefinition
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.lib.StringCollator
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.util.URI
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.value.EmptySequence
import client.net.sf.saxon.ce.value.SequenceType
import client.net.sf.saxon.ce.value.StringValue
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An xsl:sort element in the stylesheet. <br>
 */
class XSLSort extends StyleElement {

  @BeanProperty
  var sortKeyDefinition: SortKeyDefinition = _

  private var select: Expression = _

  private var order: Expression = _

  private var dataType: Expression = null

  private var caseOrder: Expression = _

  private var lang: Expression = _

  private var collationName: Expression = _

  @BeanProperty
  var stable: Expression = _

  private var useDefaultCollation: Boolean = true

  /**
   * Determine whether this type of element is allowed to contain a sequence constructor
   * @return true: yes, it may contain a sequence constructor
   */
  def mayContainSequenceConstructor(): Boolean = true

  def prepareAttributes(): Unit = {
    select = checkAttribute("select", "e").asInstanceOf[Expression]
    order = checkAttribute("order", "a").asInstanceOf[Expression]
    dataType = checkAttribute("data-type", "a").asInstanceOf[Expression]
    caseOrder = checkAttribute("case-order", "a").asInstanceOf[Expression]
    lang = checkAttribute("lang", "a").asInstanceOf[Expression]
    collationName = checkAttribute("collation", "a").asInstanceOf[Expression]
    stable = checkAttribute("stable", "a").asInstanceOf[Expression]
    if (order == null) {
      order = new StringLiteral("ascending")
    }
    if (caseOrder == null) {
      caseOrder = new StringLiteral("#default")
    } else {
      useDefaultCollation = false
    }
    if (lang == null) {
      lang = new StringLiteral(StringValue.EMPTY_STRING)
    } else {
      useDefaultCollation = false
    }
    if (collationName != null) {
      useDefaultCollation = false
    }
  }

  def validate(decl: Declaration): Unit = {
    if (select != null && hasChildNodes()) {
      compileError("An xsl:sort element with a select attribute must be empty", "XTSE1015")
    }
    if (select == null && !hasChildNodes()) {
      select = new ContextItemExpression()
    }
    if (useDefaultCollation) {
      collationName = new StringLiteral(getDefaultCollationName)
    }
    var stringCollator: StringCollator = null
    if (collationName.isInstanceOf[StringLiteral]) {
      var collationString = collationName.asInstanceOf[StringLiteral].getStringValue
      try {
        var collationURI = new URI(collationString, true)
        if (!collationURI.isAbsolute) {
          val base = new URI(getBaseURI)
          collationURI = base.resolve(collationURI.toString)
          collationString = collationURI.toString
        }
      } catch {
        case err: URI.URISyntaxException ⇒ {
          compileError("Collation name '" + collationString + "' is not a valid URI")
          collationString = NamespaceConstant.CODEPOINT_COLLATION_URI
        }
      }
      stringCollator = getConfiguration.getNamedCollation(collationString)
      if (stringCollator == null) {
        compileError("Collation " + collationString + " has not been defined", "XTDE1035")
        stringCollator = CodepointCollator.getInstance
      }
    }
    select = typeCheck(select)
    order = typeCheck(order)
    caseOrder = typeCheck(caseOrder)
    lang = typeCheck(lang)
    dataType = typeCheck(dataType)
    collationName = typeCheck(collationName)
    stable = typeCheck(stable)
    if (select != null) {
      try {
        val role = new RoleLocator(RoleLocator.INSTRUCTION, "xsl:sort/select", 0)
        select = TypeChecker.staticTypeCheck(select, SequenceType.ATOMIC_SEQUENCE, false, role)
      } catch {
        case err: XPathException ⇒ compileError(err)
      }
    }
    sortKeyDefinition = new SortKeyDefinition()
    sortKeyDefinition.setSortProperty(SortKeyDefinition.ORDER, order)
    sortKeyDefinition.setSortProperty(SortKeyDefinition.CASE_ORDER, caseOrder)
    sortKeyDefinition.setSortProperty(SortKeyDefinition.LANG, lang)
    sortKeyDefinition.setSortKey(select)
    sortKeyDefinition.setSortProperty(SortKeyDefinition.DATA_TYPE, dataType)
    sortKeyDefinition.setSortProperty(SortKeyDefinition.COLLATION, collationName)
    sortKeyDefinition.setCollation(stringCollator)
    sortKeyDefinition.setBaseURI(getBaseURI)
    sortKeyDefinition.setSortProperty(SortKeyDefinition.STABLE, stable)
    sortKeyDefinition.setBackwardsCompatible(xPath10ModeIsEnabled())
  }

  /**
   * Determine the type of item returned by this instruction (only relevant if
   * it is an instruction). Default implementation returns Type.ITEM, indicating
   * that we don't know, it might be anything. Returns null in the case of an element
   * such as xsl:sort or xsl:variable that can appear in a sequence constructor but
   * contributes nothing to the result sequence.
   * @return the item type returned
   */
  protected def getReturnedItemType(): ItemType = null

  def compile(exec: Executable, decl: Declaration): Expression = {
    if (select == null) {
      var b = compileSequenceConstructor(exec, decl)
      if (b == null) {
        b = new Literal(EmptySequence.getInstance)
      }
      b.setContainer(this)
      try {
        val visitor = makeExpressionVisitor()
        var atomizedSortKey = new Atomizer(b)
        atomizedSortKey = visitor.simplify(atomizedSortKey)
        ExpressionTool.copyLocationInfo(b, atomizedSortKey)
        sortKeyDefinition.setSortKey(atomizedSortKey)
      } catch {
        case e: XPathException ⇒ compileError(e)
      }
    }
    sortKeyDefinition = sortKeyDefinition.simplify(makeExpressionVisitor())
    null
  }
}
