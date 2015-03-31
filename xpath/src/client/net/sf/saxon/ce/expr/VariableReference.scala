// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.`type`.{AnyItemType, ItemType, TypeHierarchy}
import client.net.sf.saxon.ce.om.{Item, Sequence, SequenceIterator}
import client.net.sf.saxon.ce.orbeon.Logger
import client.net.sf.saxon.ce.pattern.NodeTest
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.{Cardinality, SequenceTool, SequenceType}

/**
 * Variable reference: a reference to a variable. This may be an XSLT-defined variable, a range
 * variable defined within the XPath expression, or a variable defined in some other static context.
 */
class VariableReference extends Expression {

  protected var binding: Binding = null

  protected var staticType: SequenceType = null

  protected var constantValue: Sequence = null

  var displayName: String = null

  private var _flattened: Boolean = false
  def isFlattened = _flattened

  private var inLoop: Boolean = true

  /**
   * Create a Variable Reference
   * @param binding the variable binding to which this variable refers
   */
  def this(binding: Binding) {
    this()
    displayName = binding.getVariableQName.getDisplayName
    fixup(binding)
  }

  /**
   * Set static type. This is a callback from the variable declaration object. As well
   * as supplying the static type, it may also supply a compile-time value for the variable.
   * As well as the type information, other static properties of the value are supplied:
   * for example, whether the value is an ordered node-set.
   * @param type the static type of the variable
   * @param value the value of the variable if this is a compile-time constant
   * @param properties static properties of the expression to which the variable is bound
   */
  def setStaticType(`type`: SequenceType, value: Sequence, properties: Int): Unit = {
    staticType = `type`
    constantValue = value
    val dependencies = getDependencies
    staticProperties = (properties & ~StaticProperty.CONTEXT_DOCUMENT_NODESET) | 
      StaticProperty.NON_CREATIVE | 
      `type`.getCardinality | 
      dependencies
  }

  /**
   * Mark an expression as being "flattened". This is a collective term that includes extracting the
   * string value or typed value, or operations such as simple value construction that concatenate text
   * nodes before atomizing. The implication of all of these is that although the expression might
   * return nodes, the identity of the nodes has no significance. This is called during type checking
   * of the parent expression. At present, only variable references take any notice of this notification.
   */
  override def setFlattened(flattened: Boolean): Unit = {
    super.setFlattened(flattened)
    this._flattened = flattened
  }

  /**
   * Type-check the expression. At this stage details of the static type must be known.
   * If the variable has a compile-time value, this is substituted for the variable reference
   */
  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    if (constantValue != null) {
      binding = null
      return Literal.makeLiteral(constantValue)
    }
    if (binding.isInstanceOf[Expression]) {
      inLoop = visitor.isLoopingSubexpression(binding.asInstanceOf[Expression])
//ORBEON XSLT
//    } else if (binding.isInstanceOf[UserFunctionParameter]) {
//      inLoop = visitor.isLoopingSubexpression(null)
    }
    this
  }

  /**
   * Type-check the expression. At this stage details of the static type must be known.
   * If the variable has a compile-time value, this is substituted for the variable reference
   */
  override def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    if (constantValue != null) {
      binding = null
      return Literal.makeLiteral(constantValue)
    }
    this
  }

  /**
   * Fix up this variable reference to a Binding object, which enables the value of the variable
   * to be located at run-time.
   */
  def fixup(binding: Binding): Unit = {
    this.binding = binding
    resetLocalStaticProperties()
  }

  /**
   * Provide additional information about the type of the variable, typically derived by analyzing
   * the initializer of the variable binding
   * @param type the item type of the variable
   * @param cardinality the cardinality of the variable
   * @param constantValue the actual value of the variable, if this is known statically, otherwise null
   * @param properties additional static properties of the variable's initializer
   * @param visitor an ExpressionVisitor
   */
  def refineVariableType(`type`: ItemType, 
      cardinality: Int, 
      constantValue: Sequence, 
      properties: Int, 
      visitor: ExpressionVisitor): Unit = {
    val th = TypeHierarchy.getInstance
    val oldItemType = getItemType
    var newItemType = oldItemType
    if (th.isSubType(`type`, oldItemType)) {
      newItemType = `type`
    }
    var newcard = cardinality & getCardinality
    if (newcard == 0) {
      newcard = getCardinality
    }
    val seqType = SequenceType.makeSequenceType(newItemType, newcard)
    setStaticType(seqType, constantValue, properties)
  }

  /**
   * Determine the data type of the expression, if possible
   *
   * @return the type of the variable, if this can be determined statically;
   *         otherwise Type.ITEM (meaning not known in advance)
   */
  def getItemType(): ItemType = {
    if (staticType == null || 
      staticType.getPrimaryType == AnyItemType.getInstance) {
      if (binding != null) {
        val st = binding.getRequiredType
        if (st != null) {
          return st.getPrimaryType
        }
      }
      AnyItemType.getInstance
    } else {
      staticType.getPrimaryType
    }
  }

  /**
   * Get the static cardinality
   */
  def computeCardinality(): Int = {
    if (staticType == null) {
      if (binding == null) {
        StaticProperty.ALLOWS_ZERO_OR_MORE
      } else if (binding.isInstanceOf[LetExpression]) {
        binding.getRequiredType.getCardinality
      } else if (binding.isInstanceOf[Assignation]) {
        StaticProperty.EXACTLY_ONE
      } else {
        binding.getRequiredType.getCardinality
      }
    } else {
      staticType.getCardinality
    }
  }

  /**
   * Determine the special properties of this expression
   *
   * @return [[StaticProperty.NON_CREATIVE]]
   */
  override def computeSpecialProperties(): Int = {
    var p = super.computeSpecialProperties()
    p |= StaticProperty.NON_CREATIVE
    if (binding.isInstanceOf[Assignation]) {
      val exp = binding.asInstanceOf[Assignation].getSequence
      if (exp != null) {
        p |= (exp.getSpecialProperties & StaticProperty.NOT_UNTYPED)
      }
    }
    if (staticType != null && !Cardinality.allowsMany(staticType.getCardinality) && 
      staticType.getPrimaryType.isInstanceOf[NodeTest]) {
      p |= StaticProperty.SINGLE_DOCUMENT_NODESET
    }
    p
  }

  /**
   * Test if this expression is the same as another expression.
   * (Note, we only compare expressions that
   * have the same static and dynamic context).
   */
  override def equals(other: Any): Boolean = {
    (other.isInstanceOf[VariableReference] && 
      binding == other.asInstanceOf[VariableReference].binding && 
      binding != null)
  }

  /**
   * get HashCode for comparing two expressions
   */
  override def hashCode(): Int = {
    if (binding == null) 73619830 else binding.hashCode
  }

  override def getIntrinsicDependencies(): Int = {
    var d = 0
    if (binding == null) {
      d |= (StaticProperty.DEPENDS_ON_LOCAL_VARIABLES | StaticProperty.DEPENDS_ON_RUNTIME_ENVIRONMENT)
    } else if (binding.isGlobal) {
//ORBEON XSLT
//      if (binding.isInstanceOf[GlobalParam]) {
//        d |= StaticProperty.DEPENDS_ON_RUNTIME_ENVIRONMENT
//      }
    } else {
      d |= StaticProperty.DEPENDS_ON_LOCAL_VARIABLES
    }
    d
  }

  /**
   * Promote this expression if possible
   */
  override def promote(offer: PromotionOffer, parent: Expression): Expression = this

  /**
   * An implementation of Expression must provide at least one of the methods evaluateItem(), iterate(), or process().
   * This method indicates which of these methods is provided. This implementation provides both all three methods
   * natively.
   */
  override def getImplementationMethod(): Int = {
    (if (Cardinality.allowsMany(getCardinality)) 0 else Expression.EVALUATE_METHOD) |
      Expression.ITERATE_METHOD |
      Expression.PROCESS_METHOD
  }

  /**
   * Get the value of this variable in a given context.
   *
   * @param c the XPathContext which contains the relevant variable bindings
   * @return the value of the variable, if it is defined
   * @throws XPathException if the variable is undefined
   */
  override def iterate(c: XPathContext): SequenceIterator = {
    try {
      evaluateVariable(c).iterate()
    } catch {
      case err: XPathException => {
        err.maybeSetLocation(getSourceLocator)
        throw err
      }
      case err: AssertionError => {
        val msg = err.getMessage + ". Variable reference $" + getDisplayName + 
          (if (getSystemId == null) "" else " of " + getSystemId)
        val logger = Logger.getLogger("VariableReference")
        logger.severe("internal null reference error: " + msg)
        throw new XPathException(msg)
      }
    }
  }

  override def evaluateItem(c: XPathContext): Item = {
    try {
      SequenceTool.asItem(evaluateVariable(c))
    } catch {
      case err: XPathException => {
        err.maybeSetLocation(getSourceLocator)
        throw err
      }
    }
  }

  override def process(c: XPathContext): Unit = {
    try {
      val actual = evaluateVariable(c)
      SequenceTool.process(actual.iterate(), c)
    } catch {
      case err: XPathException => {
        err.maybeSetLocation(getSourceLocator)
        throw err
      }
    }
  }

  /**
   * Evaluate this variable
   * @param c the XPath dynamic context
   * @return the value of the variable
   * @throws XPathException if any error occurs
   */
  def evaluateVariable(c: XPathContext): Sequence = {
    try {
      binding.evaluateVariable(c)
    } catch {
      case err: NullPointerException => if (binding == null) {
        throw new IllegalStateException("Variable $" + displayName + " has not been fixed up")
      } else {
        throw err
      }
    }
  }

  /**
   * Get the object bound to the variable
   * @return the Binding which declares this variable and associates it with a value
   */
  def getBinding(): Binding = binding

  /**
   * Get the display name of the variable. This is taken from the variable binding if possible
   * @return the display name (a lexical QName
   */
  def getDisplayName(): String = {
    if (binding != null) {
      binding.getVariableQName.getDisplayName
    } else {
      displayName
    }
  }

  /**
   * The toString() method for an expression attempts to give a representation of the expression
   * in an XPath-like form, but there is no guarantee that the syntax will actually be true XPath.
   * In the case of XSLT instructions, the toString() method gives an abstracted view of the syntax
   */
  override def toString(): String = {
    val d = getDisplayName
    "$" + (if (d == null) "$" else d)
  }
}
