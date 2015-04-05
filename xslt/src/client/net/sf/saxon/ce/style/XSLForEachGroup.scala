// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.expr.instruct.ForEachGroup
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.pattern.Pattern
import client.net.sf.saxon.ce.pattern.PatternSponsor
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.util.URI
import client.net.sf.saxon.ce.value.EmptySequence
import client.net.sf.saxon.ce.value.SequenceType
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Handler for xsl:for-each-group elements in stylesheet. This is a new instruction
 * defined in XSLT 2.0
 */
class XSLForEachGroup extends StyleElement {

  private var select: Expression = null

  private var groupBy: Expression = null

  private var groupAdjacent: Expression = null

  private var starting: Pattern = null

  private var ending: Pattern = null

  private var collationName: Expression = _

  /**
   * Determine whether this node is an instruction.
   * @return true - it is an instruction
   */
  def isInstruction(): Boolean = true

  /**
   * Specify that xsl:sort is a permitted child
   */
  protected def isPermittedChild(child: StyleElement): Boolean = child.isInstanceOf[XSLSort]

  /**
   * Determine whether this type of element is allowed to contain a template-body
   * @return true: yes, it may contain a template-body
   */
  def mayContainSequenceConstructor(): Boolean = true

  def prepareAttributes(): Unit = {
    select = checkAttribute("select", "e1").asInstanceOf[Expression]
    groupBy = checkAttribute("group-by", "e").asInstanceOf[Expression]
    groupAdjacent = checkAttribute("group-adjacent", "e").asInstanceOf[Expression]
    starting = checkAttribute("group-starting-with", "p").asInstanceOf[Pattern]
    ending = checkAttribute("group-ending-with", "p").asInstanceOf[Pattern]
    val collationAtt = checkAttribute("collation", "w").asInstanceOf[String]
    checkForUnknownAttributes()
    val c = (if (groupBy == null) 0 else 1) + (if (groupAdjacent == null) 0 else 1) + 
      (if (starting == null) 0 else 1) + 
      (if (ending == null) 0 else 1)
    if (c != 1) {
      compileError("Exactly one of the attributes group-by, group-adjacent, group-starting-with, " + 
        "and group-ending-with must be specified", "XTSE1080")
    }
    if (collationAtt != null) {
      if (groupBy == null && groupAdjacent == null) {
        compileError("A collation may be specified only if group-by or group-adjacent is specified", 
          "XTSE1090")
      } else {
        collationName = makeAttributeValueTemplate(collationAtt)
        if (collationName.isInstanceOf[StringLiteral]) {
          val collation = collationName.asInstanceOf[StringLiteral].getStringValue
          var collationURI: URI = null
          try {
            collationURI = new URI(collation, true)
            if (!collationURI.isAbsolute) {
              val base = new URI(getBaseURI)
              collationURI = base.resolve(collationURI.toString)
              collationName = new StringLiteral(collationURI.toString)
            }
          } catch {
            case err: URI.URISyntaxException ⇒
              compileError("Collation name '" + collationName + "' is not a valid URI", "XTDE1110")
              collationName = new StringLiteral(NamespaceConstant.CODEPOINT_COLLATION_URI)
          }
        }
      }
    } else {
      val defaultCollation = getDefaultCollationName
      if (defaultCollation != null) {
        collationName = new StringLiteral(defaultCollation)
      }
    }
  }

  def validate(decl: Declaration): Unit = {
    checkSortComesFirst(false)
    select = typeCheck(select)
    val visitor = makeExpressionVisitor()
    if (groupBy != null) {
      groupBy = typeCheck(groupBy)
      try {
        val role = new RoleLocator(RoleLocator.INSTRUCTION, "xsl:for-each-group/group-by", 0)
        groupBy = TypeChecker.staticTypeCheck(groupBy, SequenceType.ATOMIC_SEQUENCE, backwardsCompatible = false, role)
      } catch {
        case err: XPathException ⇒ compileError(err)
      }
    } else if (groupAdjacent != null) {
      groupAdjacent = typeCheck(groupAdjacent)
      try {
        val role = new RoleLocator(RoleLocator.INSTRUCTION, "xsl:for-each-group/group-adjacent", 0)
        role.setErrorCode("XTTE1100")
        groupAdjacent = TypeChecker.staticTypeCheck(groupAdjacent, SequenceType.SINGLE_ATOMIC, backwardsCompatible = false,
          role)
      } catch {
        case err: XPathException ⇒ compileError(err)
      }
    }
    starting = typeCheck("starting", starting)
    ending = typeCheck("ending", ending)
    if (starting != null || ending != null) {
      try {
        val role = new RoleLocator(RoleLocator.INSTRUCTION, "xsl:for-each-group/select", 0)
        role.setErrorCode("XTTE1120")
        select = TypeChecker.staticTypeCheck(select, SequenceType.NODE_SEQUENCE, backwardsCompatible = false, role)
      } catch {
        case err: XPathException ⇒
          val prefix = if (starting != null) "With group-starting-with attribute: " else "With group-ending-with attribute: "
          compileError(prefix + err.getMessage, err.getErrorCodeQName)
      }
    }
  }

  def compile(exec: Executable, decl: Declaration): Expression = {
    var algorithm = 0
    var key: Expression = null
    if (groupBy != null) {
      algorithm = ForEachGroup.GROUP_BY
      key = groupBy
    } else if (groupAdjacent != null) {
      algorithm = ForEachGroup.GROUP_ADJACENT
      key = groupAdjacent
    } else if (starting != null) {
      algorithm = ForEachGroup.GROUP_STARTING
      key = new PatternSponsor(starting)
    } else if (ending != null) {
      algorithm = ForEachGroup.GROUP_ENDING
      key = new PatternSponsor(ending)
    }
    val action = compileSequenceConstructor(exec, decl)
    if (action == null) {
      return new Literal(EmptySequence.getInstance)
    }
    try {
      new ForEachGroup(select, makeExpressionVisitor().simplify(action), algorithm, key, collationName, 
        getBaseURI, makeSortKeys(decl))
    } catch {
      case e: XPathException ⇒
        compileError(e)
        null
    }
  }
}
