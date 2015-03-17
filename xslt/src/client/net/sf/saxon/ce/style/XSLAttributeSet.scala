package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.ExpressionVisitor
import client.net.sf.saxon.ce.expr.Literal
import client.net.sf.saxon.ce.expr.instruct.AttributeSet
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.AnyItemType
import java.util.ArrayList
import java.util.Iterator
import java.util.List
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An xsl:attribute-set element in the stylesheet. <br>
 */
class XSLAttributeSet extends StyleElement with StylesheetProcedure {

  private var useAtt: String = _

  private var attributeSetElements: List[Declaration] = null

  private var useAttributeSets: Array[AttributeSet] = null

  private var procedure: AttributeSet = new AttributeSet()

  private var validated: Boolean = false

  /**
   * Ask whether this node is a declaration, that is, a permitted child of xsl:stylesheet
   * (including xsl:include and xsl:import).
   * @return true for this element
   */
  override def isDeclaration(): Boolean = true

  /**
   * Get the name of this attribute set
   * @return the name of the attribute set, as a QName
   */
  def getAttributeSetName(): StructuredQName = getObjectName

  /**
   * Get the compiled code produced for this XSLT element
   * @return the compiled AttributeSet
   */
  def getInstruction(): AttributeSet = procedure

  def prepareAttributes() {
    setObjectName(checkAttribute("name", "q1").asInstanceOf[StructuredQName])
    useAtt = checkAttribute("use-attribute-sets", "w").asInstanceOf[String]
    checkForUnknownAttributes()
  }

  /**
   * Get a name identifying the object of the expression, for example a function name, template name,
   * variable name, key name, element name, etc. This is used only where the name is known statically.
   * If there is no name, the value will be null.
   * @return the name of the object declared in this element, if any
   */
  def getObjectName(): StructuredQName = {
    var o = super.getObjectName
    if (o == null) {
      try {
        prepareAttributes()
        o = getObjectName
      } catch {
        case err: XPathException => {
          o = new StructuredQName("saxon", NamespaceConstant.SAXON, "badly-named-attribute-set")
          setObjectName(o)
        }
      }
    }
    o
  }

  def validate(decl: Declaration) {
    if (validated) return
    checkTopLevel(null)
    onlyAllow("attribute")
    if (useAtt != null) {
      attributeSetElements = new ArrayList[Declaration](5)
      useAttributeSets = getAttributeSets(useAtt, attributeSetElements)
      var it = attributeSetElements.iterator()
      while (it.hasNext) {
        it.next().getSourceElement.asInstanceOf[XSLAttributeSet]
          .checkCircularity(this)
      }
    }
    validated = true
  }

  /**
   * Check for circularity: specifically, check that this attribute set does not contain
   * a direct or indirect reference to the one supplied as a parameter
   * @param origin the place from which the search started
   */
  def checkCircularity(origin: XSLAttributeSet) {
    if (this == origin) {
      compileError("The definition of the attribute set is circular", "XTSE0720")
      useAttributeSets = null
    } else {
      if (!validated) {
        return
      }
      if (attributeSetElements != null) {
        var it = attributeSetElements.iterator()
        while (it.hasNext) {
          it.next().getSourceElement.asInstanceOf[XSLAttributeSet]
            .checkCircularity(origin)
        }
      }
    }
  }

  /**
   * Compile the attribute set
   * @param exec the Executable
   * @param decl
   * @return a Procedure object representing the compiled attribute set
   * @throws XPathException if a failure is detected
   */
  def compile(exec: Executable, decl: Declaration): Expression = {
    var body = compileSequenceConstructor(exec, decl)
    if (body == null) {
      body = Literal.makeEmptySequence()
    }
    try {
      val visitor = makeExpressionVisitor()
      body = visitor.simplify(body)
      procedure.setUseAttributeSets(useAttributeSets)
      procedure.setName(getObjectName)
      procedure.setBody(body)
      procedure.setSourceLocator(this)
      procedure.setExecutable(exec)
      val exp2 = body.optimize(visitor, AnyItemType.getInstance)
      if (body != exp2) {
        procedure.setBody(exp2)
        body = exp2
      }
      procedure.allocateSlots(0)
    } catch {
      case e: XPathException => compileError(e)
    }
    null
  }

  /**
   * Optimize the stylesheet construct
   * @param declaration
   */
  def optimize(declaration: Declaration) {
  }
}
