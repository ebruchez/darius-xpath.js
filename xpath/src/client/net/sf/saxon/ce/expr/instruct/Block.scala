// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.expr.instruct

import client.net.sf.saxon.ce.`type`._
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.expr.instruct.Block._
import client.net.sf.saxon.ce.om.{Axis, Item, SequenceIterator}
import client.net.sf.saxon.ce.orbeon._
import client.net.sf.saxon.ce.pattern.EmptySequenceTest
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter._
import client.net.sf.saxon.ce.value._

import scala.util.control.Breaks


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
      val list = new ArrayList[Expression](10)
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
      var exps = new Array[Expression](list.size)
      exps = list.toArray(exps)
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
      var exps = new Array[Expression](list.size)
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

  private var _children: Array[Expression] = _

  /**
   * Set the children of this instruction
   * @param children The instructions that are children of this instruction
   */
  def setChildren(children: Array[Expression]): Unit = {
    _children = children
    for (c <- 0 until children.length) {
      adoptChildExpression(children(c))
    }
  }

  def getChildren = _children

  def getExpressionName(): String = "sequence"

  override def computeSpecialProperties(): Int = {
    if (_children.length == 0) {
      return StaticProperty.SPECIAL_PROPERTY_MASK & ~StaticProperty.HAS_SIDE_EFFECTS
    }
    var p = super.computeSpecialProperties()
    var allAxisExpressions = true
    var allChildAxis = true
    var allSubtreeAxis = true
    import Breaks._
      breakable {
      for (i <- 0 until _children.length) {
        if (! _children(i).isInstanceOf[AxisExpression]) {
          allAxisExpressions = false
          allChildAxis = false
          allSubtreeAxis = false
          break()
        }
        val axis = _children(i).asInstanceOf[AxisExpression].getAxis
        if (axis != Axis.CHILD) {
          allChildAxis = false
        }
        if (!Axis.isSubtreeAxis(axis)) {
          allSubtreeAxis = false
        }
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
      if (_children.length == 2 &&
        _children(0).asInstanceOf[AxisExpression].getAxis == Axis.ATTRIBUTE &&
        _children(1).asInstanceOf[AxisExpression].getAxis == Axis.CHILD) {
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
    val isLiteralText = new Array[Boolean](_children.length)
    var hasAdjacentTextNodes = false
    for (i <- 0 until _children.length) {
      isLiteralText(i) = _children(i).isInstanceOf[ValueOf] &&
        _children(i).asInstanceOf[ValueOf].getContentExpression.isInstanceOf[StringLiteral]
      if (i > 0 && isLiteralText(i) && isLiteralText(i - 1)) {
        hasAdjacentTextNodes = true
      }
    }
    if (hasAdjacentTextNodes) {
      val content = new ArrayList[Expression](_children.length)
      var pendingText: String = null
      for (i <- 0 until _children.length) {
        if (isLiteralText(i)) {
          pendingText = (if (pendingText == null) "" else pendingText) + 
            _children(i).asInstanceOf[ValueOf].getContentExpression.asInstanceOf[StringLiteral]
            .getStringValue
        } else {
          if (pendingText != null) {
            val inst = new ValueOf(new StringLiteral(pendingText), false)
            content.add(inst)
            pendingText = null
          }
          content.add(_children(i))
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

  override def iterateSubExpressions(): Iterator[Expression] = Iterator(_children.iterator)

//ORBEON unused
//  /**
//   * Test whether the Block includes a LocalParam instruction (which will be true only if it is the
//   * body of an XSLT template)
//   * @return true if the Block contains a LocalParam instruction
//   */
//  def containsLocalParam(): Boolean = {
//    children.length > 0 && children(0).isInstanceOf[LocalParam]
//  }

  /**
   * Determine the data type of the items returned by this expression
   * @return the data type
   */
  override def getItemType(): ItemType = {
    if (_children.length == 0) {
      return EmptySequenceTest.getInstance
    }
    var t1 = _children(0).getItemType
    for (i <- 1 until _children.length) {
      t1 = Type.getCommonSuperType(t1, _children(i).getItemType)
      if (t1.isInstanceOf[AnyItemType]) {
        return t1
      }
    }
    t1
  }

  /**
   * Determine the cardinality of the expression
   */
  override def getCardinality(): Int = {
    if (_children.length == 0) {
      return StaticProperty.EMPTY
    }
    var c1 = _children(0).getCardinality
    for (i <- 1 until _children.length) {
      c1 = Cardinality.sum(c1, _children(i).getCardinality)
      if (c1 == StaticProperty.ALLOWS_ZERO_OR_MORE) {
        return c1
      }
    }
    c1
  }

  /**
   * Determine whether this instruction creates new nodes.
   * This implementation returns true if any child instruction
   * returns true.
   */
  override def createsNewNodes(): Boolean = {
    for (i <- 0 until _children.length) {
      val props = _children(i).getSpecialProperties
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
   * @throws XPathException if an error is discovered during expression
   *     rewriting
   * @return the simplified expression
   * @param visitor an expression visitor
   */
  override def simplify(visitor: ExpressionVisitor): Expression = {
    var allAtomic = true
    var nested = false
    for (c <- 0 until _children.length) {
      _children(c) = visitor.simplify(_children(c))
      if (!Literal.isAtomic(_children(c))) {
        allAtomic = false
      }
      if (_children(c).isInstanceOf[Block]) {
        nested = true
      } else if (Literal.isEmptySequence(_children(c))) {
        nested = true
      }
    }
    if (_children.length == 1) {
      return _children(0)
    }
    if (_children.length == 0) {
      val result = Literal.makeEmptySequence()
      ExpressionTool.copyLocationInfo(this, result)
      return result
    }
    if (nested) {
      val list = new ArrayList[Expression](_children.length * 2)
      flatten(list)
      _children = new Array[Expression](list.size)
      for (i <- 0 until _children.length) {
        _children(i) = list.get(i)
        adoptChildExpression(_children(i))
      }
    }
    if (allAtomic) {
      val values = new Array[Item](_children.length)
      for (c <- 0 until _children.length) {
        values(c) = _children(c).asInstanceOf[Literal].getValue.asInstanceOf[AtomicValue]
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
  private def flatten(targetList: List[Expression]): Unit = {
    var currentLiteralList: List[Item] = null
    for (i <- 0 until _children.length) {
      if (Literal.isEmptySequence(_children(i))) {
      } else if (_children(i).isInstanceOf[Block]) {
        flushCurrentLiteralList(currentLiteralList, targetList)
        currentLiteralList = null
        _children(i).asInstanceOf[Block].flatten(targetList)
      } else if (_children(i).isInstanceOf[Literal]) {
        val iterator = _children(i).asInstanceOf[Literal].getValue.iterate()
        if (currentLiteralList == null) {
          currentLiteralList = new ArrayList[Item](10)
        }
        import Breaks._
        breakable {
          while (true) {
            val item = iterator.next()
            if (item == null) {
              break()
            }
            currentLiteralList.add(item)
          }
        }
      } else {
        flushCurrentLiteralList(currentLiteralList, targetList)
        currentLiteralList = null
        targetList.add(_children(i))
      }
    }
    flushCurrentLiteralList(currentLiteralList, targetList)
  }

  private def flushCurrentLiteralList(currentLiteralList: List[Item], list: List[Expression]): Unit = {
    if (currentLiteralList != null) {
      val iter = new client.net.sf.saxon.ce.tree.iter.ListIterator(currentLiteralList)
      list.add(Literal.makeLiteral(SequenceExtent.makeSequenceExtent(iter)))
    }
  }

  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    for (c <- 0 until _children.length) {
      _children(c) = visitor.typeCheck(_children(c), contextItemType)
      adoptChildExpression(_children(c))
    }
    this
  }

  override def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    for (c <- 0 until _children.length) {
      _children(c) = visitor.optimize(_children(c), contextItemType)
      adoptChildExpression(_children(c))
    }
    var canSimplify = false
    var prevLiteral = false
    import Breaks._
    breakable {
      for (c <- 0 until _children.length) {
        if (_children(c).isInstanceOf[Block]) {
          canSimplify = true
          break()
        }
        if (_children(c).isInstanceOf[Literal]) {
          if (prevLiteral || Literal.isEmptySequence(_children(c))) {
            canSimplify = true
            break()
          }
          prevLiteral = true
        } else {
          prevLiteral = false
        }
      }
    }
    if (canSimplify) {
      val list = new ArrayList[Expression](_children.length * 2)
      flatten(list)
      _children = new Array[Expression](list.size)
      for (i <- 0 until _children.length) {
        _children(i) = list.get(i)
        adoptChildExpression(_children(i))
      }
    }
    if (_children.length == 0) {
      Literal.makeEmptySequence()
    } else if (_children.length == 1) {
      _children(0)
    } else {
      this
    }
  }

  /**
   * Handle promotion offers, that is, non-local tree rewrites.
   * @param offer The type of rewrite being offered
   * @throws XPathException
   */
  override protected def promoteInst(offer: PromotionOffer): Unit = {
    for (c <- 0 until _children.length) {
      _children(c) = doPromotion(_children(c), offer)
    }
  }

  def processLeavingTail(context: XPathContext): TailCall = {
    var tc: TailCall = null
    for (i <- 0 until _children.length) {
      try {
        if (_children(i).isInstanceOf[TailCallReturner]) {
          tc = _children(i).asInstanceOf[TailCallReturner].processLeavingTail(context)
        } else {
          _children(i).process(context)
          tc = null
        }
      } catch {
        case e: XPathException =>
          e.maybeSetLocation(_children(i).getSourceLocator)
          throw e
      }
    }
    tc
  }

  /**
   * An implementation of Expression must provide at least one of the methods evaluateItem(), iterate(), or process().
   * This method indicates which of these methods is provided. This implementation provides both iterate() and
   * process() methods natively.
   */
  override def getImplementationMethod(): Int = Expression.ITERATE_METHOD | Expression.PROCESS_METHOD

  /**
   * Iterate over the results of all the child expressions
   */
  override def iterate(context: XPathContext): SequenceIterator = {
    if (_children.length == 0) {
      EmptyIterator.getInstance
    } else if (_children.length == 1) {
      _children(0).iterate(context)
    } else {
      new BlockIterator(_children, context)
    }
  }
}
