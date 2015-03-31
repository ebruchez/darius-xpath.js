// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.expr.sort.CodepointCollator
import client.net.sf.saxon.ce.lib.StringCollator
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.pattern.Pattern
import client.net.sf.saxon.ce.trans.Err
import client.net.sf.saxon.ce.trans.KeyDefinition
import client.net.sf.saxon.ce.trans.KeyManager
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.util.URI
import client.net.sf.saxon.ce.`type`.AtomicType
import client.net.sf.saxon.ce.value.SequenceType
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Handler for xsl:key elements in stylesheet. <br>
 */
class XSLKey extends StyleElement with StylesheetProcedure {

  private var `match`: Pattern = _

  private var use: Expression = _

  private var collationName: String = _

  override def isDeclaration(): Boolean = true

  /**
   * Determine whether this type of element is allowed to contain a sequence constructor
   * @return true: yes, it may contain a sequence constructor
   */
  def mayContainSequenceConstructor(): Boolean = true

  def prepareAttributes(): Unit = {
    setObjectName(checkAttribute("name", "q1").asInstanceOf[StructuredQName])
    use = checkAttribute("use", "e").asInstanceOf[Expression]
    `match` = checkAttribute("match", "p1").asInstanceOf[Pattern]
    collationName = checkAttribute("collation", "w").asInstanceOf[String]
    checkForUnknownAttributes()
  }

  def getKeyName(): StructuredQName = getObjectName

  def validate(decl: Declaration): Unit = {
    checkTopLevel(null)
    if (use != null) {
      if (hasChildNodes()) {
        compileError("An xsl:key element with a @use attribute must be empty", "XTSE1205")
      }
      try {
        val role = new RoleLocator(RoleLocator.INSTRUCTION, "xsl:key/use", 0)
        use = TypeChecker.staticTypeCheck(use, SequenceType.makeSequenceType(AtomicType.ANY_ATOMIC, StaticProperty.ALLOWS_ZERO_OR_MORE), 
          false, role)
      } catch {
        case err: XPathException ⇒ compileError(err)
      }
    } else {
      if (!hasChildNodes()) {
        compileError("An xsl:key element must either have a @use attribute or have content", "XTSE1205")
      }
    }
    use = typeCheck(use)
    `match` = typeCheck("match", `match`)
    if (use != null) {
      use = makeExpressionVisitor().typeCheck(use, `match`.getNodeTest)
    }
    if (collationName != null) {
      var collationURI: URI = null
      try {
        collationURI = new URI(collationName, true)
        if (!collationURI.isAbsolute) {
          val base = new URI(getBaseURI)
          collationURI = base.resolve(collationURI.toString)
          collationName = collationURI.toString
        }
      } catch {
        case err: URI.URISyntaxException ⇒ compileError("Collation name '" + collationName + "' is not a valid URI")
      }
    } else {
      collationName = getDefaultCollationName
    }
  }

  protected def index(decl: Declaration, top: PrincipalStylesheetModule): Unit = {
    val keyName = getKeyName
    if (keyName != null) {
      top.getExecutable.getKeyManager.preRegisterKeyDefinition(keyName)
    }
  }

  def compile(exec: Executable, decl: Declaration): Expression = {
    val env = getStaticContext
    var collator: StringCollator = null
    if (collationName != null) {
      collator = getConfiguration.getNamedCollation(collationName)
      if (collator == null) {
        compileError("The collation name " + Err.wrap(collationName, Err.URI) + 
          " is not recognized", "XTSE1210")
        collator = CodepointCollator.getInstance
      }
      if (collator.isInstanceOf[CodepointCollator]) {
        collator = null
        collationName = null
      } else {
        compileError("The collation used for xsl:key must be capable of generating collation keys", "XTSE1210")
      }
    }
    if (use == null) {
      val body = compileSequenceConstructor(exec, decl)
      try {
        val visitor = makeExpressionVisitor()
        use = new Atomizer(body)
        use = visitor.simplify(use)
      } catch {
        case e: XPathException ⇒ compileError(e)
      }
      try {
        val role = new RoleLocator(RoleLocator.INSTRUCTION, "xsl:key/use", 0)
        use = TypeChecker.staticTypeCheck(use, SequenceType.makeSequenceType(AtomicType.ANY_ATOMIC, StaticProperty.ALLOWS_ZERO_OR_MORE), 
          false, role)
        use = makeExpressionVisitor().typeCheck(use, `match`.getNodeTest)
      } catch {
        case err: XPathException ⇒ compileError(err)
      }
    }
    var useType = use.getItemType.asInstanceOf[AtomicType]
    if (xPath10ModeIsEnabled()) {
      if (useType != AtomicType.STRING && useType != AtomicType.UNTYPED_ATOMIC) {
        use = new AtomicSequenceConverter(use, AtomicType.STRING)
        useType = AtomicType.STRING
      }
    }
    val slots = `match`.allocateSlots(0)
    allocatePatternSlots(slots)
    val km = getExecutable.getKeyManager
    val keydef = new KeyDefinition(`match`, use, collationName, collator)
    keydef.setIndexedItemType(useType)
    keydef.setSourceLocator(this)
    keydef.setExecutable(getExecutable)
    keydef.setBackwardsCompatible(xPath10ModeIsEnabled())
    keydef.allocateSlots(0)
    try {
      km.addKeyDefinition(getObjectName, keydef, exec.getConfiguration)
    } catch {
      case err: XPathException ⇒ compileError(err)
    }
    null
  }

  /**
   * Optimize the stylesheet construct
   * @param declaration
   */
  def optimize(declaration: Declaration): Unit = {
  }
}
