package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.expr.sort.GenericAtomicComparer
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om._
import client.net.sf.saxon.ce.pattern.AnyNodeTest
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter.UnfailingIterator
import client.net.sf.saxon.ce.tree.util.Navigator
import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.value.AtomicValue
import client.net.sf.saxon.ce.value.BooleanValue
import DeepEqual._
//remove if not needed
import scala.collection.JavaConversions._

object DeepEqual {

  /**
   * Determine when two sequences are deep-equal
   *
   * @param op1 the first sequence
   * @param op2 the second sequence
   * @param collator the collator to be used
   * @return true if the sequences are deep-equal
   * @throws XPathException if either sequence contains a function item
   */
  private def deepEquals(op1: SequenceIterator, op2: SequenceIterator, collator: GenericAtomicComparer): Boolean = {
    var result = true
    try {
      while (true) {
        val item1 = op1.next()
        val item2 = op2.next()
        if (item1 == null && item2 == null) {
          //break
        }
        if (item1 == null || item2 == null) {
          result = false
          //break
        }
        if (item1.isInstanceOf[NodeInfo]) {
          if (item2.isInstanceOf[NodeInfo]) {
            if (!deepEquals(item1.asInstanceOf[NodeInfo], item2.asInstanceOf[NodeInfo], collator)) {
              result = false
              //break
            }
          } else {
            result = false
            //break
          }
        } else {
          if (item2.isInstanceOf[NodeInfo]) {
            result = false
            //break
          } else {
            val av1 = item1.asInstanceOf[AtomicValue]
            val av2 = item2.asInstanceOf[AtomicValue]
            if (av1.isNaN && av2.isNaN) {
            } else if (!collator.comparesEqual(av1, av2)) {
              result = false
              //break
            }
          }
        }
      }
    } catch {
      case err: ClassCastException => result = false
      case err: XPathException => {
        if ("FOTY0015" == err.getErrorCodeLocalPart && NamespaceConstant.ERR == err.getErrorCodeNamespace) {
          throw err
        }
        result = false
      }
    }
    result
  }

  /**
   * Determine whether two nodes are deep-equal
   */
  private def deepEquals(n1: NodeInfo, n2: NodeInfo, comparer: GenericAtomicComparer): Boolean = {
    if (n1.isSameNodeInfo(n2)) return true
    if (n1.getNodeKind != n2.getNodeKind) {
      return false
    }
    n1.getNodeKind match {
      case Type.ELEMENT => 
        if (n1.getNodeName != n2.getNodeName) {
          false
        }
        var a1 = n1.iterateAxis(Axis.ATTRIBUTE, AnyNodeTest.getInstance)
        var a2 = n2.iterateAxis(Axis.ATTRIBUTE, AnyNodeTest.getInstance)
        if (Count.count(a1.getAnother) != Count.count(a2)) {
          false
        }
        while (true) {
          val att1 = a1.next().asInstanceOf[NodeInfo]
          if (att1 == null) {
            //break
          }
          val val2 = Navigator.getAttributeValue(n2, att1.getURI, att1.getLocalPart)
          if (val2 == null) {
            return false
          }
          if (!comparer.getCollator.comparesEqual(att1.getStringValue, val2)) {
            false
          }
        }

      case Type.DOCUMENT => 
        var c1 = n1.iterateAxis(Axis.CHILD, AnyNodeTest.getInstance)
        var c2 = n2.iterateAxis(Axis.CHILD, AnyNodeTest.getInstance)
        while (true) {
          var d1 = c1.next().asInstanceOf[NodeInfo]
          while (d1 != null && isIgnorable(d1)) {
            d1 = c1.next().asInstanceOf[NodeInfo]
          }
          var d2 = c2.next().asInstanceOf[NodeInfo]
          while (d2 != null && isIgnorable(d2)) {
            d2 = c2.next().asInstanceOf[NodeInfo]
          }
          if (d1 == null || d2 == null) {
            return (d1 == d2)
          }
          if (!deepEquals(d1, d2, comparer)) {
            false
          }
        }

      case Type.ATTRIBUTE | Type.PROCESSING_INSTRUCTION | Type.NAMESPACE | Type.TEXT | Type.COMMENT => 
        var s1 = n1.getNodeName
        var s2 = n2.getNodeName
        ((if (s1 == null) s2 == null else s1 == s2) && 
          comparer.comparesEqual(n1.getTypedValue, n2.getTypedValue))

      case _ => throw new IllegalArgumentException("Unknown node type")
    }
  }

  private def isIgnorable(node: NodeInfo): Boolean = {
    val kind = node.getNodeKind
    if (kind == Type.COMMENT) {
      return true
    } else if (kind == Type.PROCESSING_INSTRUCTION) {
      return true
    }
    false
  }
}

/**
 * XSLT 2.0 deep-equal() function.
 * Supports deep comparison of two sequences (of nodes and/or atomic values)
 * optionally using a collation
 */
class DeepEqual extends CollatingFunction {

  def newInstance(): DeepEqual = new DeepEqual()

  /**
   * Evaluate the expression
   */
  def evaluateItem(context: XPathContext): Item = {
    val collator = getAtomicComparer(2, context)
    val op1 = argument(0).iterate(context)
    val op2 = argument(1).iterate(context)
    try {
      BooleanValue.get(deepEquals(op1, op2, collator))
    } catch {
      case e: XPathException => {
        e.maybeSetLocation(getSourceLocator)
        throw e
      }
    }
  }
}
