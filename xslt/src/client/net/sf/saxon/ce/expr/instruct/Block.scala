package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.om.Axis
import client.net.sf.saxon.ce.tree.iter._
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.pattern.EmptySequenceTest
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`._
import client.net.sf.saxon.ce.value._
import java.util._
import Block._
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

object Block {

  /**
   * Static factory method to create a block. If one of the arguments is already a block,
   * the contents will be merged into a new composite block
   * @param e1 the first subexpression (child) of the block
   * @param e2 the second subexpression (child) of the block
   * @return a Block containing the two subexpressions, and if either of them is a block, it will
   * have been collapsed to create a flattened sequence
   */
  def makeBlock(e1: Expression, e2: Expression): Expression = {
    if (e1 == null || Literal.isEmptySequence(e1)) {
      return e2
    }
    if (e2 == null || Literal.isEmptySequence(e2)) {
      return e1
    }
    if (e1.isInstanceOf[Block] || e2.isInstanceOf[Block]) {
      val list = new ArrayList(10)
      if (e1.isInstanceOf[Block]) {
        val it1 = e1.iterateSubExpressions()
        while (it1.hasNext) {
          list.add(it1.next())
        }
      } else {
        list.add(e1)
      }
      if (e2.isInstanceOf[Block]) {
        val it2 = e2.iterateSubExpressions()
        while (it2.hasNext) {
          list.add(it2.next())
        }
      } else {
        list.add(e2)
      }
      var exps = Array.ofDim[Expression](list.size)
      exps = list.toArray(exps).asInstanceOf[Array[Expression]]
      val b = new Block()
      b.setChildren(exps)
      b
    } else {
      val exps = Array(e1, e2)
      val b = new Block()
      b.setChildren(exps)
      b
    }
  }

  /**
   * Static factory method to create a block from a list of expressions
   * @param list the list of expressions making up this block. The members of the List must
   * be instances of Expression
   * @return a Block containing the two subexpressions, and if either of them is a block, it will
   * have been collapsed to create a flattened sequence
   */
  def makeBlock(list: List[Expression]): Expression = {
    if (list.size == 0) {
      Literal.makeEmptySequence()
    } else if (list.size == 1) {
      list.get(0)
    } else {
      var exps = Array.ofDim[Expression](list.size)
      exps = list.toArray(exps)
      val b = new Block()
      b.setChildren(exps)
      b
    }
  }
}

/**
 * An expression that delivers the concatenation of the results of its subexpressions. This may
 * represent an XSLT sequence constructor, or an XPath/XQuery expression of the form (a,b,c,d).
 */
class Block extends Instruction {

  @BeanProperty
  var children: Array[Expression] = _

  /**
   * Set the children of this instruction
   * @param children The instructions that are children of this instruction
   */
  def setChildren(children: Array[Expression]) {
    this.children = children
    for (c <- 0 until children.length) {
      adoptChildExpression(children(c))
    }
  }

  def getExpressionName(): String = "sequence"

  def computeSpecialProperties(): Int = {
    if (children.length == 0) {
      return StaticProperty.SPECIAL_PROPERTY_MASK & ~StaticProperty.HAS_SIDE_EFFECTS
    }
    var p = super.computeSpecialProperties()
    var allAxisExpressions = true
    var allChildAxis = true
    var allSubtreeAxis = true
    for (i <- 0 until children.length) {
      if (!(children(i).isInstanceOf[AxisExpression])) {
        allAxisExpressions = false
        allChildAxis = false
        allSubtreeAxis = false
        //break
      }
      val axis = children(i).asInstanceOf[AxisExpression].getAxis
      if (axis != Axis.CHILD) {
        allChildAxis = false
      }
      if (!Axis.isSubtreeAxis(axis)) {
        allSubtreeAxis = false
      }
    }
    if (allAxisExpressions) {
      p |= StaticProperty.CONTEXT_DOCUMENT_NODESET | StaticProperty.SINGLE_DOCUMENT_NODESET | 
        StaticProperty.NON_CREATIVE
      if (allChildAxis) {
        p |= StaticProperty.PEER_NODESET
      }
      if (allSubtreeAxis) {
        p |= StaticProperty.SUBTREE_NODESET
      }
      if (children.length == 2 && 
        children(0).asInstanceOf[AxisExpression].getAxis == Axis.ATTRIBUTE && 
        children(1).asInstanceOf[AxisExpression].getAxis == Axis.CHILD) {
        p |= StaticProperty.ORDERED_NODESET
      }
    }
    p
  }

  /**
   * Merge any adjacent instructions that create literal text nodes
   * @return the expression after merging literal text instructions
   */
  def mergeAdjacentTextInstructions(): Expression = {
    val isLiteralText = Array.ofDim[Boolean](children.length)
    var hasAdjacentTextNodes = false
    for (i <- 0 until children.length) {
      isLiteralText(i) = children(i).isInstanceOf[ValueOf] && 
        children(i).asInstanceOf[ValueOf].getContentExpression.isInstanceOf[StringLiteral]
      if (i > 0 && isLiteralText(i) && isLiteralText(i - 1)) {
        hasAdjacentTextNodes = true
      }
    }
    if (hasAdjacentTextNodes) {
      val content = new ArrayList(children.length)
      var pendingText: String = null
      for (i <- 0 until children.length) {
        if (isLiteralText(i)) {
          pendingText = (if (pendingText == null) "" else pendingText) + 
            children(i).asInstanceOf[ValueOf].getContentExpression.asInstanceOf[StringLiteral]
            .getStringValue
        } else {
          if (pendingText != null) {
            val inst = new ValueOf(new StringLiteral(pendingText), false)
            content.add(inst)
            pendingText = null
          }
          content.add(children(i))
        }
      }
      if (pendingText != null) {
        val inst = new ValueOf(new StringLiteral(pendingText), false)
        content.add(inst)
      }
      makeBlock(content)
    } else {
      this
    }
  }

  def iterateSubExpressions(): Iterator[Expression] = Arrays.asList(children:_*).iterator()

  /**
   * Test whether the Block includes a LocalParam instruction (which will be true only if it is the
   * body of an XSLT template)
   * @return true if the Block contains a LocalParam instruction
   */
  def containsLocalParam(): Boolean = {
    children.length > 0 && children(0).isInstanceOf[LocalParam]
  }

  /**
   * Determine the data type of the items returned by this expression
   * @return the data type
   */
  def getItemType(): ItemType = {
    if (children.length == 0) {
      return EmptySequenceTest.getInstance
    }
    var t1 = children(0).getItemType
    for (i <- 1 until children.length) {
      t1 = Type.getCommonSuperType(t1, children(i).getItemType)
      if (t1.isInstanceOf[AnyItemType]) {
        return t1
      }
    }
    t1
  }

  /**
   * Determine the cardinality of the expression
   */
  def getCardinality(): Int = {
    if (children.length == 0) {
      return StaticProperty.EMPTY
    }
    var c1 = children(0).getCardinality
    for (i <- 1 until children.length) {
      c1 = Cardinality.sum(c1, children(i).getCardinality)
      if (c1 == StaticProperty.ALLOWS_ZERO_OR_MORE) {
        //break
      }
    }
    c1
  }

  /**
   * Determine whether this instruction creates new nodes.
   * This implementation returns true if any child instruction
   * returns true.
   */
  def createsNewNodes(): Boolean = {
    for (i <- 0 until children.length) {
      val props = children(i).getSpecialProperties
      if ((props & StaticProperty.NON_CREATIVE) == 0) {
        return true
      }
    }
    false
  }

  /**
   * Simplify an expression. This performs any static optimization (by rewriting the expression
   * as a different expression). The default implementation does nothing.
   *
   * @exception XPathException if an error is discovered during expression
   *     rewriting
   * @return the simplified expression
   * @param visitor an expression visitor
   */
  def simplify(visitor: ExpressionVisitor): Expression = {
    var allAtomic = true
    var nested = false
    for (c <- 0 until children.length) {
      children(c) = visitor.simplify(children(c))
      if (!Literal.isAtomic(children(c))) {
        allAtomic = false
      }
      if (children(c).isInstanceOf[Block]) {
        nested = true
      } else if (Literal.isEmptySequence(children(c))) {
        nested = true
      }
    }
    if (children.length == 1) {
      return getChildren()(0)
    }
    if (children.length == 0) {
      val result = Literal.makeEmptySequence()
      ExpressionTool.copyLocationInfo(this, result)
      return result
    }
    if (nested) {
      val list = new ArrayList(children.length * 2)
      flatten(list)
      children = Array.ofDim[Expression](list.size)
      for (i <- 0 until children.length) {
        children(i) = list.get(i).asInstanceOf[Expression]
        adoptChildExpression(children(i))
      }
    }
    if (allAtomic) {
      val values = Array.ofDim[AtomicValue](children.length)
      for (c <- 0 until children.length) {
        values(c) = children(c).asInstanceOf[Literal].getValue.asInstanceOf[AtomicValue]
      }
      val result = Literal.makeLiteral(new SequenceExtent(values))
      ExpressionTool.copyLocationInfo(this, result)
      return result
    }
    this
  }

  /**
   * Simplify the contents of a Block by merging any nested blocks, merging adjacent
   * literals, and eliminating any empty sequences.
   * @param targetList the new list of expressions comprising the contents of the block
   * after simplification
   * @throws XPathException should not happen
   */
  private def flatten(targetList: List[Expression]) {
    var currentLiteralList: List[Item] = null
    for (i <- 0 until children.length) {
      if (Literal.isEmptySequence(children(i))) {
      } else if (children(i).isInstanceOf[Block]) {
        flushCurrentLiteralList(currentLiteralList, targetList)
        currentLiteralList = null
        children(i).asInstanceOf[Block].flatten(targetList)
      } else if (children(i).isInstanceOf[Literal]) {
        val iterator = children(i).asInstanceOf[Literal].getValue.iterate()
        if (currentLiteralList == null) {
          currentLiteralList = new ArrayList[Item](10)
        }
        while (true) {
          val item = iterator.next()
          if (item == null) {
            //break
          }
          currentLiteralList.add(item)
        }
      } else {
        flushCurrentLiteralList(currentLiteralList, targetList)
        currentLiteralList = null
        targetList.add(children(i))
      }
    }
    flushCurrentLiteralList(currentLiteralList, targetList)
  }

  private def flushCurrentLiteralList(currentLiteralList: List[Item], list: List[Expression]) {
    if (currentLiteralList != null) {
      val iter = new client.net.sf.saxon.ce.tree.iter.ListIterator(currentLiteralList)
      list.add(Literal.makeLiteral(SequenceExtent.makeSequenceExtent(iter)))
    }
  }

  def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    for (c <- 0 until children.length) {
      children(c) = visitor.typeCheck(children(c), contextItemType)
      adoptChildExpression(children(c))
    }
    this
  }

  def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    for (c <- 0 until children.length) {
      children(c) = visitor.optimize(children(c), contextItemType)
      adoptChildExpression(children(c))
    }
    var canSimplify = false
    var prevLiteral = false
    for (c <- 0 until children.length) {
      if (children(c).isInstanceOf[Block]) {
        canSimplify = true
        //break
      }
      if (children(c).isInstanceOf[Literal]) {
        if (prevLiteral || Literal.isEmptySequence(children(c))) {
          canSimplify = true
          //break
        }
        prevLiteral = true
      } else {
        prevLiteral = false
      }
    }
    if (canSimplify) {
      val list = new ArrayList(children.length * 2)
      flatten(list)
      children = Array.ofDim[Expression](list.size)
      for (i <- 0 until children.length) {
        children(i) = list.get(i).asInstanceOf[Expression]
        adoptChildExpression(children(i))
      }
    }
    if (children.length == 0) {
      Literal.makeEmptySequence()
    } else if (children.length == 1) {
      children(0)
    } else {
      this
    }
  }

  /**
   * Handle promotion offers, that is, non-local tree rewrites.
   * @param offer The type of rewrite being offered
   * @throws XPathException
   */
  protected def promoteInst(offer: PromotionOffer) {
    for (c <- 0 until children.length) {
      children(c) = doPromotion(children(c), offer)
    }
  }

  def processLeavingTail(context: XPathContext): TailCall = {
    var tc: TailCall = null
    for (i <- 0 until children.length) {
      try {
        if (children(i).isInstanceOf[TailCallReturner]) {
          tc = children(i).asInstanceOf[TailCallReturner].processLeavingTail(context)
        } else {
          children(i).process(context)
          tc = null
        }
      } catch {
        case e: XPathException => {
          e.maybeSetLocation(children(i).getSourceLocator)
          throw e
        }
      }
    }
    tc
  }

  /**
   * An implementation of Expression must provide at least one of the methods evaluateItem(), iterate(), or process().
   * This method indicates which of these methods is provided. This implementation provides both iterate() and
   * process() methods natively.
   */
  def getImplementationMethod(): Int = ITERATE_METHOD | PROCESS_METHOD

  /**
   * Iterate over the results of all the child expressions
   */
  def iterate(context: XPathContext): SequenceIterator = {
    if (children.length == 0) {
      EmptyIterator.getInstance
    } else if (children.length == 1) {
      children(0).iterate(context)
    } else {
      new BlockIterator(children, context)
    }
  }
}
