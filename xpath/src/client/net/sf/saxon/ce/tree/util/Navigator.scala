package client.net.sf.saxon.ce.tree.util

import client.net.sf.saxon.ce.Controller
import client.net.sf.saxon.ce.event.Receiver
import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.ItemMappingFunction
import client.net.sf.saxon.ce.expr.UnfailingItemMappingIterator
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.functions.Count
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om._
import client.net.sf.saxon.ce.pattern._
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.iter._
import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.value.SequenceExtent
import java.util.ArrayList
import java.util.List
//remove if not needed
import scala.collection.JavaConversions._

object Navigator {

  /**
   * Helper method to get an attribute of an element
   * @param element the element node
   * @param uri the attribute name URI
   * @param local the local name of the attribute
   */
  def getAttributeValue(element: NodeInfo, uri: String, local: String): String = {
    val test = new NameTest(Type.ATTRIBUTE, uri, local)
    val iterator = element.iterateAxis(Axis.ATTRIBUTE, test)
    val attribute = iterator.next().asInstanceOf[NodeInfo]
    if (attribute == null) {
      null
    } else {
      attribute.getStringValue
    }
  }

  /**
   * Helper method to get the base URI of an element or processing instruction node
   * @param node the node whose base URI is required
   * @return the base URI of the node
   * @since 8.7
   */
  def getBaseURI(node: NodeInfo): String = {
    val xmlBase = getAttributeValue(node, NamespaceConstant.XML, "base")
    if (xmlBase != null) {
      var baseURI: URI = null
      try {
        baseURI = new URI(xmlBase, true)
        if (!baseURI.isAbsolute) {
          val parent = node.getParent
          if (parent == null) {
            val base = new URI(node.getSystemId)
            val resolved = (if (xmlBase.length == 0) base else base.resolve(baseURI.toString))
            return resolved.toString
          }
          val startSystemId = node.getSystemId
          val parentSystemId = parent.getSystemId
          val base = new URI(if (startSystemId == parentSystemId) parent.getBaseURI else startSystemId)
          baseURI = (if (xmlBase.length == 0) base else base.resolve(baseURI.toString))
        }
      } catch {
        case e: URI.URISyntaxException => return xmlBase
      }
      return baseURI.toString
    }
    val startSystemId = node.getSystemId
    val parent = node.getParent
    if (parent == null) {
      return startSystemId
    }
    val parentSystemId = parent.getSystemId
    if (startSystemId == parentSystemId) {
      parent.getBaseURI
    } else {
      startSystemId
    }
  }

  /**
   * Get an absolute XPath expression that identifies a given node within its document
   *
   * @param node the node whose path is required. If null is supplied,
   *             an empty string is returned - this fact is used in making a recursive call
   *             for a parentless node.
   * @return a path expression that can be used to retrieve the node
   */
  def getPath(node: NodeInfo): String = {
    if (node == null) {
      return ""
    }
    var pre: String = null
    val parent = node.getParent
    node.getNodeKind match {
      case Type.DOCUMENT => "/"
      case Type.ELEMENT => if (parent == null) {
        node.getDisplayName
      } else {
        pre = getPath(parent)
        if (pre == "/") {
          '/' + node.getDisplayName
        } else {
          var position = 1
          var count = 0
          val siblings = parent.iterateAxis(Axis.CHILD, new NameTest(node))
          while (true) {
            val sib = siblings.next().asInstanceOf[NodeInfo]
            if (sib == null) {
              //break
            }
            count += 1
            if (sib.isSameNodeInfo(node)) {
              position = count
            }
          }
          try {
            val index = (if (count == 1) "" else "[" + position + "]")
            pre + '/' + node.getDisplayName + index
          } catch {
            case e: UnsupportedOperationException => pre + '/' + node.getDisplayName
          }
        }
      }
      case Type.ATTRIBUTE => getPath(parent) + "/@" + node.getDisplayName
      case Type.TEXT => 
        pre = getPath(parent)
        (if (pre == "/") "" else pre) + "/text()[" + getNumberSimple(node) + 
          ']'

      case Type.COMMENT => 
        pre = getPath(parent)
        (if (pre == "/") "" else pre) + "/comment()[" + getNumberSimple(node) + 
          ']'

      case Type.PROCESSING_INSTRUCTION => 
        pre = getPath(parent)
        (if (pre == "/") "" else pre) + "/processing-instruction()[" + 
          getNumberSimple(node) + 
          ']'

      case Type.NAMESPACE => 
        var test = node.getLocalPart
        if (test.length == 0) {
          test = "*[not(local-name()]"
        }
        getPath(parent) + "/namespace::" + test

      case _ => ""
    }
  }

  /**
   * Get simple node number. This is defined as one plus the number of previous siblings of the
   * same node type and name. It is not accessible directly in XSL.
   *
   *
   * @param node    The node whose number is required
   * @return the node number, as defined above
   */
  def getNumberSimple(node: NodeInfo): Int = {
    var same: NodeTest = null
    same = if (node.getNodeName == null) NodeKindTest.makeNodeKindTest(node.getNodeKind) else new NameTest(node)
    val preceding = node.iterateAxis(Axis.PRECEDING_SIBLING, same)
    Count.count(preceding) + 1
  }

  /**
   * Get node number (level="single"). If the current node matches the supplied pattern, the returned
   * number is one plus the number of previous siblings that match the pattern. Otherwise,
   * return the element number of the nearest ancestor that matches the supplied pattern.
   *
   * @param node    the current node, the one whose node number is required
   * @param count   Pattern that identifies which nodes should be
   *                counted. Default (null) is the element name if the current node is
   *                an element, or "node()" otherwise.
   * @param from    Pattern that specifies where counting starts from.
   *                Default (null) is the root node. (This parameter does not seem
   *                useful but is included for the sake of XSLT conformance.)
   * @param context the dynamic context of the transformation, used if
   *                the patterns reference context values (e.g. variables)
   * @return the node number established as follows: go to the nearest
   *         ancestor-or-self that matches the 'count' pattern and that is a
   *         descendant of the nearest ancestor that matches the 'from' pattern.
   *         Return one plus the nunber of preceding siblings of that ancestor
   *         that match the 'count' pattern. If there is no such ancestor,
   *         return 0.
   * @throws XPathException when any error occurs in processing
   */
  def getNumberSingle(node: NodeInfo, 
      count: Pattern, 
      from: Pattern, 
      context: XPathContext): Int = {
    if (count == null && from == null) {
      return getNumberSimple(node)
    }
    var knownToMatch = false
    if (count == null) {
      count = if (node.getNodeName == null) new NodeTestPattern(NodeKindTest.makeNodeKindTest(node.getNodeKind)) else new NodeTestPattern(new NameTest(node))
      knownToMatch = true
    }
    var target = node
    while (!(knownToMatch || count.matches(target, context))) {
      target = target.getParent
      if (target == null) {
        return 0
      }
      if (from != null && from.matches(target, context)) {
        return 0
      }
    }
    val preceding = target.iterateAxis(Axis.PRECEDING_SIBLING, count.getNodeTest)
    val alreadyChecked = (count.isInstanceOf[NodeTestPattern])
    var i = 1
    while (true) {
      val p = preceding.next().asInstanceOf[NodeInfo]
      if (p == null) {
        return i
      }
      if (alreadyChecked || count.matches(p, context)) {
        i += 1
      }
    }
  }

  /**
   * Get node number (level="any").
   * Return one plus the number of previous nodes in the
   * document that match the supplied pattern
   *
   * @param inst                   Identifies the xsl:number expression; this is relevant
   *                               when the function is memoised to support repeated use of the same
   *                               instruction to number multiple nodes
   * @param node                   The node being numbered
   * @param count                  Pattern that identifies which nodes should be
   *                               counted. Default (null) is the element name if the current node is
   *                               an element, or "node()" otherwise.
   * @param from                   Pattern that specifies where counting starts from.
   *                               Default (null) is the root node. Only nodes at or after the first (most
   *                               recent) node that matches the 'from' pattern are counted.
   * @param context                The dynamic context for the transformation
   * @param hasVariablesInPatterns if the count or from patterns
   *                               contain variables, then it's not safe to get the answer by adding
   *                               one to the number of the most recent node that matches
   * @return one plus the number of nodes that precede the current node,
   *         that match the count pattern, and that follow the first node that
   *         matches the from pattern if specified.
   * @throws XPathException if an error occurs matching a pattern
   *
   */
  def getNumberAny(inst: Expression, 
      node: NodeInfo, 
      count: Pattern, 
      from: Pattern, 
      context: XPathContext, 
      hasVariablesInPatterns: Boolean): Int = {
    var memoNode: NodeInfo = null
    var memoNumber = 0
    val controller = context.getController
    val memoise = (!hasVariablesInPatterns) && from == null
    if (memoise) {
      val memo = controller.getUserData(inst, "xsl:number").asInstanceOf[Array[Any]]
      if (memo != null) {
        memoNode = memo(0).asInstanceOf[NodeInfo]
        memoNumber = memo(1).asInstanceOf[java.lang.Integer]
      }
    }
    var num = 0
    if (count == null) {
      count = if (node.getNodeName == null) new NodeTestPattern(NodeKindTest.makeNodeKindTest(node.getNodeKind)) else new NodeTestPattern(new NameTest(node))
      num = 1
    } else if (count.matches(node, context)) {
      num = 1
    }
    var filter: NodeTest = null
    filter = if (from == null) count.getNodeTest else if (from.getNodeKind == Type.ELEMENT && count.getNodeKind == Type.ELEMENT) NodeKindTest.ELEMENT else AnyNodeTest.getInstance
    if (from != null && from.matches(node, context)) {
      return num
    }
    var preceding = new PrecedingEnumeration(node, true)
    if (filter != AnyNodeTest.getInstance) {
      preceding = newAxisFilter(preceding, filter)
    }
    while (true) {
      val prev = preceding.next().asInstanceOf[NodeInfo]
      if (prev == null) {
        //break
      }
      if (count.matches(prev, context)) {
        if (num == 1 && memoNode != null && prev.isSameNodeInfo(memoNode)) {
          num = memoNumber + 1
          //break
        }
        num += 1
      }
      if (from != null && from.matches(prev, context)) {
        //break
      }
    }
    if (memoise) {
      val memo = Array.ofDim[Any](2)
      memo(0) = node
      memo(1) = num
      controller.setUserData(inst, "xsl:number", memo)
    }
    num
  }

  /**
   * Get node number (level="multiple").
   * Return a vector giving the hierarchic position of this node. See the XSLT spec for details.
   *
   * @param node    The node to be numbered
   * @param count   Pattern that identifies which nodes (ancestors and
   *                their previous siblings) should be counted. Default (null) is the
   *                element name if the current node is an element, or "node()"
   *                otherwise.
   * @param from    Pattern that specifies where counting starts from.
   *                Default (null) is the root node. Only nodes below the first (most
   *                recent) node that matches the 'from' pattern are counted.
   * @param context The dynamic context for the transformation
   * @return a list containing for each ancestor-or-self that matches the
   *         count pattern and that is below the nearest node that matches the
   *         from pattern, an Integer which is one greater than the number of
   *         previous siblings that match the count pattern.
   * @throws XPathException if an error occurs matching the pattern
   */
  def getNumberMulti(node: NodeInfo, 
      count: Pattern, 
      from: Pattern, 
      context: XPathContext): List[Integer] = {
    val v = new ArrayList[Integer](5)
    if (count == null) {
      count = if (node.getNodeName == null) new NodeTestPattern(NodeKindTest.makeNodeKindTest(node.getNodeKind)) else new NodeTestPattern(new NameTest(node))
    }
    var curr = node
    while (true) {
      if (count.matches(curr, context)) {
        val num = getNumberSingle(curr, count, null, context)
        v.add(0, num)
      }
      curr = curr.getParent
      if (curr == null) {
        //break
      }
      if (from != null && from.matches(curr, context)) {
        //break
      }
    }
    v
  }

  /**
   * Generic (model-independent) implementation of deep copy algorithm for nodes.
   * This is available for use by any node implementations that choose to use it.
   *
   *
   * @param node            The node to be copied
   * @param out             The receiver to which events will be sent
   * @param copyOptions     Options for copying namespaces, type annotations, etc,
   *                        as defined in {@link client.net.sf.saxon.ce.om.CopyOptions}
   * @throws XPathException on any failure reported by the Receiver
   */
  def copy(node: NodeInfo, out: Receiver, copyOptions: Int) node.getNodeKind match {
    case Type.DOCUMENT => {
      out.startDocument()
      val children0 = node.iterateAxis(Axis.CHILD, AnyNodeTest.getInstance)
      while (true) {
        val child = children0.next().asInstanceOf[NodeInfo]
        if (child == null) {
          //break
        }
        child.copy(out, copyOptions)
      }
      out.endDocument()
      //break
    }
    case Type.ELEMENT => {
      out.startElement(node.getNodeName, 0)
      if ((copyOptions & CopyOptions.LOCAL_NAMESPACES) != 0) {
        val localNamespaces = node.getDeclaredNamespaces(null)
        for (i <- 0 until localNamespaces.length) {
          val ns = localNamespaces(i)
          if (ns == null) {
            //break
          }
          out.namespace(ns, 0)
        }
      } else if ((copyOptions & CopyOptions.ALL_NAMESPACES) != 0) {
        NamespaceIterator.sendNamespaces(node, out)
      }
      val attributes = node.iterateAxis(Axis.ATTRIBUTE, AnyNodeTest.getInstance)
      while (true) {
        val att = attributes.next().asInstanceOf[NodeInfo]
        if (att == null) {
          //break
        }
        att.copy(out, copyOptions)
      }
      out.startContent()
      val children = node.iterateAxis(Axis.CHILD, AnyNodeTest.getInstance)
      while (true) {
        val child = children.next().asInstanceOf[NodeInfo]
        if (child == null) {
          //break
        }
        child.copy(out, copyOptions)
      }
      out.endElement()
      return
    }
    case Type.ATTRIBUTE => {
      out.attribute(node.getNodeName, node.getStringValue)
      return
    }
    case Type.TEXT => {
      val value = node.getStringValue
      if (value.length != 0) {
        out.characters(value)
      }
      return
    }
    case Type.COMMENT => {
      out.comment(node.getStringValue)
      return
    }
    case Type.PROCESSING_INSTRUCTION => {
      out.processingInstruction(node.getLocalPart, node.getStringValue)
      return
    }
    case Type.NAMESPACE => {
      out.namespace(new NamespaceBinding(node.getLocalPart, node.getStringValue), 0)
      return
    }
    case _}

  /**
   * Generic (model-independent) method to determine the relative position of two
   * node in document order. The nodes must be in the same tree.
   *
   * @param first  The first node
   * @param second The second node, whose position is to be compared with the first node
   * @return -1 if this node precedes the other node, +1 if it follows the other
   *         node, or 0 if they are the same node. (In this case, isSameNode() will always
   *         return true, and the two nodes will produce the same result for generateId())
   */
  def compareOrder(first: NodeInfo, second: NodeInfo): Int = {
    if (first.isSameNodeInfo(second)) {
      return 0
    }
    val firstParent = first.getParent
    if (firstParent == null) {
      return -1
    }
    val secondParent = second.getParent
    if (secondParent == null) {
      return +1
    }
    if (firstParent.isSameNodeInfo(secondParent)) {
      val cat1 = nodeCategories(first.getNodeKind)
      val cat2 = nodeCategories(second.getNodeKind)
      if (cat1 == cat2) {
        return first.getSiblingPosition - second.getSiblingPosition
      } else {
        return cat1 - cat2
      }
    }
    var depth1 = 0
    var depth2 = 0
    var p1 = first
    var p2 = second
    while (p1 != null) {
      depth1 += 1
      p1 = p1.getParent
    }
    while (p2 != null) {
      depth2 += 1
      p2 = p2.getParent
    }
    p1 = first
    while (depth1 > depth2) {
      p1 = p1.getParent
      if (p1.isSameNodeInfo(second)) {
        return +1
      }
      depth1 -= 1
    }
    p2 = second
    while (depth2 > depth1) {
      p2 = p2.getParent
      if (p2.isSameNodeInfo(first)) {
        return -1
      }
      depth2 -= 1
    }
    while (true) {
      val par1 = p1.getParent
      val par2 = p2.getParent
      if (par1 == null || par2 == null) {
        throw new NullPointerException("Node order comparison - internal error")
      }
      if (par1.isSameNodeInfo(par2)) {
        if (p1.getNodeKind == Type.ATTRIBUTE && p2.getNodeKind != Type.ATTRIBUTE) {
          return -1
        }
        if (p1.getNodeKind != Type.ATTRIBUTE && p2.getNodeKind == Type.ATTRIBUTE) {
          return +1
        }
        return p1.getSiblingPosition - p2.getSiblingPosition
      }
      p1 = par1
      p2 = par2
    }
  }

  /**
   * Classify node kinds into categories for sorting into document order:
   * 0 = document, 1 = namespace, 2 = attribute, 3 = (element, text, comment, pi)
   */
  private var nodeCategories: Array[Int] = Array(-1, 3, 2, 3, -1, -1, -1, 3, 3, 0, -1, -1, -1, 1)

  /**
   * Get a character string that uniquely identifies this node and that collates nodes
   * into document order
   * @param node the node whose unique identifier is reuqired
   * @param sb a buffer to which the unique identifier will be appended
   * @param addDocNr true if a unique document number is to be included in the information
   */
  def appendSequentialKey(node: NodeInfo, sb: FastStringBuffer, addDocNr: Boolean) {
    if (addDocNr) {
      sb.append('w')
      sb.append(Long toString node.getDocumentNumber)
    }
    if (node.getNodeKind != Type.DOCUMENT) {
      val parent = node.getParent
      if (parent != null) {
        appendSequentialKey(parent, sb, false)
      }
    }
    sb.append(alphaKey(node.getSiblingPosition))
    node.getNodeKind match {
      case Type.ATTRIBUTE => sb.append('A')
      case Type.NAMESPACE => sb.append('N')
      case Type.TEXT => sb.append('T')
      case Type.COMMENT => sb.append('C')
      case Type.PROCESSING_INSTRUCTION => sb.append('P')
    }
  }

  /**
   * Construct an alphabetic key from an positive integer; the key collates in the same sequence
   * as the integer
   *
   * @param value The positive integer key value (negative values are treated as zero).
   * @return the alphabetic key value
   */
  def alphaKey(value: Int): String = {
    if (value < 1) {
      return "a"
    }
    if (value < 10) {
      return "b" + value
    }
    if (value < 100) {
      return "c" + value
    }
    if (value < 1000) {
      return "d" + value
    }
    if (value < 10000) {
      return "e" + value
    }
    if (value < 100000) {
      return "f" + value
    }
    if (value < 1000000) {
      return "g" + value
    }
    if (value < 10000000) {
      return "h" + value
    }
    if (value < 100000000) {
      return "i" + value
    }
    if (value < 1000000000) {
      return "j" + value
    }
    "k" + value
  }

  /**
   * Test if one node is an ancestor-or-self of another
   *
   * @param a the putative ancestor-or-self node
   * @param d the putative descendant node
   * @return true if a is an ancestor-or-self of d
   */
  def isAncestorOrSelf(a: NodeInfo, d: NodeInfo): Boolean = {
    var p = d
    while (p != null) {
      if (a.isSameNodeInfo(p)) {
        return true
      }
      p = p.getParent
    }
    false
  }

  /**
   * Create an iterator over a singleton node, if it exists and matches a nodetest;
   * otherwise return an empty iterator
   * @param node the singleton node, or null if the node does not exist
   * @param nodeTest the test to be applied
   * @return an iterator over the node if it exists and matches the test.
   */
  def filteredSingleton(node: NodeInfo, nodeTest: NodeTest): UnfailingIterator = {
    if (node != null && nodeTest.matchesItem(node)) {
      SingletonIterator.makeIterator(node)
    } else {
      EmptyIterator.getInstance
    }
  }

  def newAxisFilter(base: UnfailingIterator, test: NodeTest): UnfailingIterator = {
    if (test == AnyNodeTest.getInstance) {
      return base
    }
    val umf = new ItemMappingFunction() {

      def mapItem(item: Item): Item = {
        return (if (test.matchesItem(item)) item else null)
      }
    }
    new UnfailingItemMappingIterator(base, umf)
  }

  /**
   * EmptyTextFilter is an iterator that applies removes any zero-length text
   * nodes returned by an underlying AxisIterator.
   */
  def newEmptyTextFilter(base: UnfailingIterator): UnfailingIterator = {
    val umf = new ItemMappingFunction() {

      def mapItem(item: Item): Item = {
        if (item.asInstanceOf[NodeInfo].getNodeKind == Type.TEXT && 
          item.getStringValue == "") {
          return null
        }
        return item
      }
    }
    new UnfailingItemMappingIterator(base, umf)
  }

  def getAncestorIterator(origin: NodeInfo, nodeTest: NodeTest, includeSelf: Boolean): UnfailingIterator = {
    new SteppingIterator(origin, new ParentFunction(nodeTest), includeSelf)
  }

  /**
   * General-purpose implementation of the descendant and descendant-or-self axes,
   * in terms of the child axis.
   * But it also has the option to return the descendants in reverse document order;
   * this is used when evaluating the preceding axis. Note that the includeSelf option
   * should not be used when scanning in reverse order, as the self node will always be
   * returned first.
   */
  class DescendantEnumeration(var start: NodeInfo, var includeSelf: Boolean, var forwards: Boolean)
      extends UnfailingIterator {

    private var children: UnfailingIterator = null

    private var descendants: UnfailingIterator = null

    private var atEnd: Boolean = false

    def next(): Item = {
      if (descendants != null) {
        val nextd = descendants.next().asInstanceOf[NodeInfo]
        if (nextd != null) {
          return nextd
        } else {
          descendants = null
        }
      }
      if (children != null) {
        val n = children.next().asInstanceOf[NodeInfo]
        if (n != null) {
          if (n.hasChildNodes()) {
            if (forwards) {
              descendants = new DescendantEnumeration(n, false, forwards)
              n
            } else {
              descendants = new DescendantEnumeration(n, true, forwards)
              next()
            }
          } else {
            n
          }
        } else {
          if (forwards || !includeSelf) {
            null
          } else {
            atEnd = true
            children = null
            start
          }
        }
      } else if (atEnd) {
        null
      } else {
        if (start.hasChildNodes()) {
          children = start.iterateAxis(Axis.CHILD, AnyNodeTest.getInstance)
          if (!forwards) {
            val forwards = start.iterateAxis(Axis.CHILD, AnyNodeTest.getInstance)
            var reversed: SequenceExtent = null
            reversed = SequenceExtent.makeReversed(forwards)
            children = reversed.iterate()
          }
        } else {
          children = EmptyIterator.getInstance
        }
        if (forwards && includeSelf) {
          start
        } else {
          next()
        }
      }
    }

    def getAnother(): UnfailingIterator = {
      new DescendantEnumeration(start, includeSelf, forwards)
    }
  }

  /**
   * General purpose implementation of the following axis, in terms of the
   * ancestor, child, and following-sibling axes
   */
  class FollowingEnumeration(var start: NodeInfo) extends UnfailingIterator {

    private var ancestorEnum: UnfailingIterator = getAncestorIterator(start, AnyNodeTest.getInstance, 
      false)

    private var siblingEnum: UnfailingIterator = null

    private var descendEnum: UnfailingIterator = null

    start.getNodeKind match {
      case Type.ELEMENT | Type.TEXT | Type.COMMENT | Type.PROCESSING_INSTRUCTION => siblingEnum = start.iterateAxis(Axis.FOLLOWING_SIBLING, 
        AnyNodeTest.getInstance)
      case Type.ATTRIBUTE | Type.NAMESPACE => 
        var parent = start.getParent
        siblingEnum = if (parent == null) EmptyIterator.getInstance else parent.iterateAxis(Axis.CHILD, 
          AnyNodeTest.getInstance)

      case _ => siblingEnum = EmptyIterator.getInstance
    }

    def next(): Item = {
      if (descendEnum != null) {
        val nextd = descendEnum.next().asInstanceOf[NodeInfo]
        if (nextd != null) {
          return nextd
        } else {
          descendEnum = null
        }
      }
      if (siblingEnum != null) {
        val nextSib = siblingEnum.next().asInstanceOf[NodeInfo]
        if (nextSib != null) {
          descendEnum = if (nextSib.hasChildNodes()) new DescendantEnumeration(nextSib, false, true) else null
          return nextSib
        } else {
          descendEnum = null
          siblingEnum = null
        }
      }
      val nexta = ancestorEnum.next().asInstanceOf[NodeInfo]
      if (nexta != null) {
        val n = nexta
        siblingEnum = if (n.getNodeKind == Type.DOCUMENT) EmptyIterator.getInstance else n.iterateAxis(Axis.FOLLOWING_SIBLING, 
          AnyNodeTest.getInstance)
        next()
      } else {
        null
      }
    }

    def getAnother(): UnfailingIterator = new FollowingEnumeration(start)
  }

  /**
   * Helper method to iterate over the preceding axis, or Saxon's internal
   * preceding-or-ancestor axis, by making use of the ancestor, descendant, and
   * preceding-sibling axes.
   */
  class PrecedingEnumeration(var start: NodeInfo, var includeAncestors: Boolean)
      extends UnfailingIterator {

    private var ancestorEnum: UnfailingIterator = getAncestorIterator(start, AnyNodeTest.getInstance, 
      false)

    private var siblingEnum: UnfailingIterator = null

    private var descendEnum: UnfailingIterator = null

    start.getNodeKind match {
      case Type.ELEMENT | Type.TEXT | Type.COMMENT | Type.PROCESSING_INSTRUCTION => siblingEnum = start.iterateAxis(Axis.PRECEDING_SIBLING, 
        AnyNodeTest.getInstance)
      case _ => siblingEnum = EmptyIterator.getInstance
    }

    def next(): Item = {
      if (descendEnum != null) {
        val nextd = descendEnum.next().asInstanceOf[NodeInfo]
        if (nextd != null) {
          return nextd
        } else {
          descendEnum = null
        }
      }
      if (siblingEnum != null) {
        val nexts = siblingEnum.next().asInstanceOf[NodeInfo]
        if (nexts != null) {
          if (nexts.hasChildNodes()) {
            descendEnum = new DescendantEnumeration(nexts, true, false)
            return next()
          } else {
            descendEnum = null
            return nexts
          }
        } else {
          descendEnum = null
          siblingEnum = null
        }
      }
      val nextAnc = ancestorEnum.next().asInstanceOf[NodeInfo]
      if (nextAnc != null) {
        siblingEnum = if (nextAnc.getNodeKind == Type.DOCUMENT) EmptyIterator.getInstance else nextAnc.iterateAxis(Axis.PRECEDING_SIBLING, 
          AnyNodeTest.getInstance)
        (if (includeAncestors) nextAnc else next())
      } else {
        null
      }
    }

    def getAnother(): UnfailingIterator = {
      new PrecedingEnumeration(start, includeAncestors)
    }
  }

  class ParentFunction(var predicate: NodeTest) extends SteppingIterator.SteppingFunction {

    def step(current: Item): Item = {
      current.asInstanceOf[NodeInfo].getParent
    }

    def conforms(current: Item): Boolean = predicate.matchesItem(current)
  }
}
