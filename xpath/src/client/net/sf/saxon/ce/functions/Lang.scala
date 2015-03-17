package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.ExpressionVisitor
import client.net.sf.saxon.ce.expr.StaticProperty
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.util.Navigator
import client.net.sf.saxon.ce.`type`.AtomicType
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.value.BooleanValue
import Lang._
//remove if not needed
import scala.collection.JavaConversions._

object Lang {

  /**
   * Test whether the context node has the given language attribute
   * @param arglang the language being tested
   * @param target the target node
   */
  def isLang(arglang: String, target: NodeInfo): Boolean = {
    var doclang: String = null
    var node = target
    while (node != null) {
      doclang = Navigator.getAttributeValue(node, NamespaceConstant.XML, "lang")
      if (doclang != null) {
        //break
      }
      node = node.getParent
      if (node == null) {
        return false
      }
    }
    if (doclang == null) {
      return false
    }
    while (true) {
      if (arglang.equalsIgnoreCase(doclang)) {
        return true
      }
      val hyphen = doclang.indexOf("-")
      if (hyphen < 0) {
        return false
      }
      doclang = doclang.substring(0, hyphen)
    }
  }
}

class Lang extends SystemFunction {

  def newInstance(): Lang = new Lang()

  def typeCheck(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    if (argument.length == 1) {
      if (contextItemType == null) {
        typeError("The context item for lang() is undefined", "XPDY0002")
      } else if (contextItemType.isInstanceOf[AtomicType]) {
        typeError("The context item for lang() is not a node", "XPDY0002")
      }
    }
    super.typeCheck(visitor, contextItemType)
  }

  /**
   * Evaluate in a general context
   */
  def evaluateItem(c: XPathContext): Item = {
    var target: NodeInfo = null
    if (argument.length > 1) {
      target = argument(1).evaluateItem(c).asInstanceOf[NodeInfo]
    } else {
      val current = c.getContextItem
      if (current == null) {
        dynamicError("The context item for lang() is undefined", "XPDY0002")
      }
      if (!(current.isInstanceOf[NodeInfo])) {
        dynamicError("The context item for lang() is not a node", "XPDY0002")
      }
      target = current.asInstanceOf[NodeInfo]
    }
    val arg0Val = argument(0).evaluateItem(c)
    val testLang = (if (arg0Val == null) "" else arg0Val.getStringValue)
    val b = isLang(testLang, target)
    BooleanValue.get(b)
  }

  /**
   * Determine the dependencies
   */
  def getIntrinsicDependencies(): Int = {
    (if (argument.length == 1) StaticProperty.DEPENDS_ON_CONTEXT_ITEM else 0)
  }
}
