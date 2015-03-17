package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.ExpressionVisitor
import client.net.sf.saxon.ce.expr.instruct.AnalyzeString
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.linked.NodeImpl
import client.net.sf.saxon.ce.`type`.ItemType
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An xsl:analyze-string elements in the stylesheet. New at XSLT 2.0<BR>
 */
class XSLAnalyzeString extends StyleElement {

  private var select: Expression = _

  private var regex: Expression = _

  private var flags: Expression = _

  private var matching: StyleElement = _

  private var nonMatching: StyleElement = _

  /**
   * Determine whether this node is an instruction.
   *
   * @return true - it is an instruction
   */
  def isInstruction(): Boolean = true

  /**
   * Determine the type of item returned by this instruction (only relevant if
   * it is an instruction).
   *
   * @return the item type returned
   */
  protected def getReturnedItemType(): ItemType = getCommonChildItemType

  def prepareAttributes() {
    select = checkAttribute("select", "e1").asInstanceOf[Expression]
    regex = checkAttribute("regex", "a1").asInstanceOf[Expression]
    flags = checkAttribute("flags", "a").asInstanceOf[Expression]
    if (flags == null) {
      flags = makeAttributeValueTemplate("")
    }
    checkForUnknownAttributes()
  }

  def validate(decl: Declaration) {
    var state = 0
    for (child <- allChildren()) {
      if (child.isInstanceOf[XSLFallback]) {
        state = 3
      } else if (child.isInstanceOf[XSLMatchingSubstring]) {
        val b = child.getLocalPart == "matching-substring"
        if (b) {
          if (state != 0) {
            outOfOrder("XTSE0010")
          }
          state = 1
          matching = child.asInstanceOf[StyleElement]
        } else {
          if (state >= 2) {
            outOfOrder("XTSE0010")
          }
          state = 2
          nonMatching = child.asInstanceOf[StyleElement]
        }
      } else {
        outOfOrder("XTSE0010")
      }
    }
    if (matching == null && nonMatching == null) {
      outOfOrder("XTSE1130")
    }
    select = typeCheck(select)
    regex = typeCheck(regex)
    flags = typeCheck(flags)
  }

  private def outOfOrder(code: String) {
    compileError("Content model for xsl:analyze-string is (xsl:matching-substring? xsl:non-matching-substring? xsl:fallback*)", 
      code)
  }

  def compile(exec: Executable, decl: Declaration): Expression = {
    var matchingBlock: Expression = null
    if (matching != null) {
      matchingBlock = matching.compileSequenceConstructor(exec, decl)
    }
    var nonMatchingBlock: Expression = null
    if (nonMatching != null) {
      nonMatchingBlock = nonMatching.compileSequenceConstructor(exec, decl)
    }
    try {
      val visitor = makeExpressionVisitor()
      new AnalyzeString(select, regex, flags, (if (matchingBlock == null) null else matchingBlock.simplify(visitor)), 
        (if (nonMatchingBlock == null) null else nonMatchingBlock.simplify(visitor)))
    } catch {
      case e: XPathException => {
        compileError(e)
        null
      }
    }
  }
}
