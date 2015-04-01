// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.trans

import client.net.sf.saxon.ce.Controller
import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.expr.sort.LocalOrderComparer
import client.net.sf.saxon.ce.lib.StringCollator
import client.net.sf.saxon.ce.om._
import client.net.sf.saxon.ce.orbeon.Configuration
import client.net.sf.saxon.ce.pattern.Pattern
import client.net.sf.saxon.ce.tree.iter.EmptyIterator
import client.net.sf.saxon.ce.tree.iter.ListIterator
import client.net.sf.saxon.ce.`type`.AtomicType
import client.net.sf.saxon.ce.`type`.Type
import client.net.sf.saxon.ce.value.AtomicValue
import client.net.sf.saxon.ce.value.DoubleValue
import client.net.sf.saxon.ce.value.NumericValue
import java.util._
import KeyManager._
//remove if not needed
import scala.collection.JavaConversions._

object KeyManager {

  private def getCollationKey(value: AtomicValue, 
      itemType: AtomicType, 
      collation: StringCollator, 
      context: XPathContext): AnyRef = {
    var `val`: AnyRef = null
    `val` = if (itemType == AtomicType.STRING || itemType == AtomicType.UNTYPED_ATOMIC || 
      itemType == AtomicType.ANY_URI) if (collation == null) value.getStringValue else collation.getCollationKey(value.getStringValue) else value.getXPathComparable(ordered = false,
      collation, context.getImplicitTimezone)
    `val`
  }
}

/**
 * KeyManager manages the set of key definitions in a stylesheet, and the indexes
 * associated with these key definitions. It handles xsl:sort-key as well as xsl:key
 * definitions.
 *
 * <p>The memory management in this class is subtle, with extensive use of weak references.
 * The idea is that an index should continue to exist in memory so long as both the compiled
 * stylesheet and the source document exist in memory: if either is removed, the index should
 * go too. The document itself holds no reference to the index. The compiled stylesheet (which
 * owns the KeyManager) holds a weak reference to the index. The index, of course, holds strong
 * references to the nodes in the document. The Controller holds a strong reference to the
 * list of indexes used for each document, so that indexes remain in memory for the duration
 * of a transformation even if the documents themselves are garbage collected.</p>
 *
 * <p>Potentially there is a need for more than one index for a given key name, depending
 * on the primitive type of the value provided to the key() function. An index is built
 * corresponding to the type of the requested value; if subsequently the key() function is
 * called with the same name and a different type of value, then a new index is built.</p>
 *
 * <p>For XSLT-defined keys, equality matching follows the rules of the eq operator, which means
 * that untypedAtomic values are treated as strings. In backwards compatibility mode, <i>all</i>
 * values are converted to strings.</p>
 *
 * <p>This class is also used for internal indexes constructed (a) to support the idref() function,
 * and (b) (in Saxon-EE only) to support filter expressions of the form /a/b/c[d=e], where the
 * path expression being filtered must be a single-document context-free path rooted at a document node,
 * where exactly one of d and e must be dependent on the focus, and where certain other conditions apply
 * such as the filter predicate not being positional. The operator in this case may be either "=" or "eq".
 * If it is "eq", then the semantics are very similar to xsl:key indexes, except that use of non-comparable
 * types gives an error rather than a non-match. If the operator is "=", however, then the rules for
 * handling untypedAtomic values are different: these must be converted to the type of the other operand.
 * In this situation the following rules apply. Assume that the predicate is [use=value], where use is
 * dependent on the focus (the indexed value), and value is the sought value.</p>
 *
 * <ul>
 * <li>If value is a type other than untypedAtomic, say T, then we build an index for type T, in which any
 * untypedAtomic values that arise in evaluating "use" are converted to type T. A conversion failure results
 * in an error. A value of a type that is not comparable to T also results in an error.</li>
 * <li>If value is untypedAtomic, then we build an index for every type actually encountered in evaluating
 * the use expression (treating untypedAtomic as string), and then search each of these indexes. (Note that
 * it is not an error if the use expression returns a mixture of say numbers and dates, provided that the
 * sought value is untypedAtomic).</li>
 * </ul>
 *
 * @author Michael H. Kay
 */
class KeyManager {

  private class IndexId(var keyName: StructuredQName, var primitiveType: AtomicType)
      {

    override def equals(o: Any): Boolean = {
      o.isInstanceOf[IndexId] && keyName == o.asInstanceOf[IndexId].keyName && 
        primitiveType == o.asInstanceOf[IndexId].primitiveType
    }

    override def hashCode(): Int = {
      keyName.hashCode ^ primitiveType.hashCode
    }
  }

  private var keyMap: HashMap[StructuredQName, KeyDefinitionSet] = new HashMap[StructuredQName, KeyDefinitionSet](10)

  @transient private var docIndexes: HashMap[DocumentInfo, HashMap[IndexId, Any]] = new HashMap[DocumentInfo, HashMap[IndexId, Any]](10)

  /**
   * Pre-register a key definition. This simply registers that a key with a given name exists,
   * without providing any details.
   * @param keyName the name of the key to be pre-registered
   */
  def preRegisterKeyDefinition(keyName: StructuredQName): Unit = {
    var keySet = keyMap.get(keyName)
    if (keySet == null) {
      keySet = new KeyDefinitionSet(keyName, keyMap.size)
      keyMap.put(keyName, keySet)
    }
  }

  /**
   * Register a key definition. Note that multiple key definitions with the same name are
   * allowed
   * @param keyName Structured QName representing the name of the key
   * @param keydef The details of the key's definition
   * @param config The configuration
   * @throws XPathException if this key definition is inconsistent with existing key definitions having the same name
   */
  def addKeyDefinition(keyName: StructuredQName, keydef: KeyDefinition, config: Configuration): Unit = {
    var keySet = keyMap.get(keyName)
    if (keySet == null) {
      keySet = new KeyDefinitionSet(keyName, keyMap.size)
      keyMap.put(keyName, keySet)
    }
    keySet.addKeyDefinition(keydef)
    val backwardsCompatible = keySet.isBackwardsCompatible
    if (backwardsCompatible) {
      val v = keySet.getKeyDefinitions
      for (i ← 0 until v.size) {
        val kd = v.get(i).asInstanceOf[KeyDefinition]
        kd.setBackwardsCompatible(true)
        if (kd.getBody.getItemType != AtomicType.STRING) {
          val exp = new AtomicSequenceConverter(kd.getBody, AtomicType.STRING)
          kd.setBody(exp)
        }
      }
    }
  }

  /**
   * Get all the key definitions that match a particular name
   * @param qName The name of the required key
   * @return The set of key definitions of the named key if there are any, or null otherwise.
   */
  def getKeyDefinitionSet(qName: StructuredQName): KeyDefinitionSet = keyMap.get(qName)

  /**
   * Build the index for a particular document for a named key
   * @param keySet The set of key definitions with this name
   * @param itemType the type of the values to be indexed.
   * @param foundItemTypes Optional (may be null). If supplied, a set that is to be populated with
   * the set of primitive types actually found among the "use" values.
   * @param doc The source document in question
   * @param context The dynamic context
   * @return the index in question, as a HashMap mapping a key value onto a ArrayList of nodes
   */
  private def buildIndex(keySet: KeyDefinitionSet, 
      itemType: AtomicType, 
      foundItemTypes: Set[AtomicType], 
      doc: DocumentInfo, 
      context: XPathContext): HashMap = {
    synchronized {
      val definitions = keySet.getKeyDefinitions
      val index = new HashMap[Any, List[NodeInfo]](100)
      for (k ← 0 until definitions.size) {
        constructIndex(doc, index, definitions.get(k), itemType, foundItemTypes, context, k == 0)
      }
      index
    }
  }

  /**
   * Process one key definition to add entries to an index
   * @param doc the document to be indexed
   * @param index the index to be built
   * @param keydef the key definition used to build the index
   * @param soughtItemType the primitive type of the value that the user is searching for on the call
   * to the key() function that triggered this index to be built
   * @param foundItemTypes Optional (may be null): if supplied, a Set to be populated with the set of
   * primitive types actually found for the use expression
   * @param context the XPath dynamic evaluation context
   * @param isFirst true if this is the first index to be built for this key
   */
  private def constructIndex(doc: DocumentInfo, 
      index: HashMap[Any, List[NodeInfo]], 
      keydef: KeyDefinition, 
      soughtItemType: AtomicType, 
      foundItemTypes: Set[AtomicType], 
      context: XPathContext, 
      isFirst: Boolean): Unit = {
    val `match` = keydef.getMatch
    val xc = context.newContext()
    xc.openStackFrame(keydef.getNumberOfSlots)
    val iter = `match`.selectNodes(doc, xc)
    while (true) {
      val item = iter.next()
      if (item == null) {
        //break
      }
      processKeyNode(item.asInstanceOf[NodeInfo], soughtItemType, foundItemTypes, keydef, index, xc, 
        isFirst)
    }
  }

  /**
   * Process one matching node, adding entries to the index if appropriate
   * @param curr the node being processed
   * @param soughtItemType the primitive item type of the argument to the key() function that triggered
   * this index to be built
   * @param foundItemTypes Optional (may be null): if supplied, a Set to be populated with the set of
   * primitive types actually found for the use expression
   * @param keydef the key definition
   * @param index the index being constructed
   * @param xc the context for evaluating expressions
   * @param isFirst indicates whether this is the first key definition with a given key name (which means
   * no sort of the resulting key entries is required)
   */
  private def processKeyNode(curr: NodeInfo, 
      soughtItemType: AtomicType, 
      foundItemTypes: Set[AtomicType], 
      keydef: KeyDefinition, 
      index: HashMap[Any, List[NodeInfo]], 
      xc: XPathContext, 
      isFirst: Boolean): Unit = {
    xc.setSingletonFocus(curr)
    val collation = keydef.getCollation
    val use = keydef.getUse
    val useval = use.iterate(xc)
    val tz = xc.getImplicitTimezone
    while (true) {
      val item = useval.next().asInstanceOf[AtomicValue]
      if (item == null) {
        //break
      }
      val actualItemType = item.getItemType
      if (foundItemTypes != null) {
        foundItemTypes.add(actualItemType)
      }
      if (!Type.isComparable(actualItemType, soughtItemType, ordered = false)) {
        //continue
      }
      var `val`: AnyRef = null
      if (soughtItemType == AtomicType.UNTYPED_ATOMIC || soughtItemType == AtomicType.STRING || 
        soughtItemType == AtomicType.ANY_URI) {
        `val` = if (collation == null) item.getStringValue else collation.getCollationKey(item.getStringValue)
      } else {
        if (item.isNaN) {
          //break
        }
        try {
          val av = item.convert(soughtItemType).asAtomic()
          `val` = av.getXPathComparable(false, collation, tz)
        } catch {
          case err: XPathException ⇒ //break
        }
      }
      var nodes = index.get(`val`)
      if (nodes == null) {
        nodes = new ArrayList[NodeInfo](4)
        index.put(`val`, nodes)
        nodes.add(curr)
      } else {
        if (isFirst) {
          if (nodes.get(nodes.size - 1) != curr) {
            nodes.add(curr)
          }
        } else {
          val comparer = LocalOrderComparer.getInstance
          var found = false
          for (i ← 0 until nodes.size) {
            val d = comparer.compare(curr, nodes.get(i))
            if (d <= 0) {
              if (d == 0) {
              } else {
                nodes.add(i, curr)
              }
              found = true
              //break
            }
          }
          if (!found) {
            nodes.add(curr)
          }
        }
      }
    }
  }

  /**
   * Get the nodes with a given key value
   * @param keySet The set of key definitions identified by the key name used in the call to the key() function
   * @param doc The source document in question
   * @param soughtValue The required key value
   * @param context The dynamic context, needed only the first time when the key is being built
   * @return an iteration of the selected nodes, always in document order with no duplicates
   */
  def selectByKey(keySet: KeyDefinitionSet, 
      doc: DocumentInfo, 
      soughtValue: AtomicValue, 
      context: XPathContext): SequenceIterator = {
    if (soughtValue == null) {
      return EmptyIterator.getInstance
    }
    val definitions = keySet.getKeyDefinitions
    val definition = definitions.get(0).asInstanceOf[KeyDefinition]
    val collation = definition.getCollation
    val keyName = keySet.getKeyName
    if (keySet.isBackwardsCompatible) {
      soughtValue = soughtValue.convert(AtomicType.STRING).asAtomic()
    } else {
      val itemType = soughtValue.getItemType
      if (itemType == AtomicType.INTEGER || itemType == AtomicType.DECIMAL || 
        itemType == AtomicType.FLOAT) {
        soughtValue = new DoubleValue(soughtValue.asInstanceOf[NumericValue].getDoubleValue)
      }
    }
    val foundItemTypes: HashSet[AtomicType] = null
    var value = soughtValue
    val keySetNumber = keySet.getKeySetNumber
    val itemType = value.getItemType
    var index: HashMap = null
    val indexObject = getIndex(doc, keyName, itemType)
    if (indexObject.isInstanceOf[String]) {
      val de = new XPathException("Key definition is circular")
      de.setErrorCode("XTDE0640")
      throw de
    }
    index = indexObject.asInstanceOf[HashMap]
    if (index == null) {
      putIndex(doc, keyName, itemType, "Under Construction", context)
      index = buildIndex(keySet, itemType, foundItemTypes, doc, context)
      putIndex(doc, keyName, itemType, index, context)
      if (foundItemTypes != null) {
        var f = foundItemTypes.iterator()
        while (f.hasNext) {
          val t = f.next()
          if (t != AtomicType.STRING) {
            putIndex(doc, keyName, t, "Under Construction", context)
            index = buildIndex(keySet, t, null, doc, context)
            putIndex(doc, keyName, t, index, context)
          }
        }
      }
    }
    if (foundItemTypes == null) {
      val nodes = index.get(getCollationKey(value, itemType, collation, context)).asInstanceOf[ArrayList]
      if (nodes == null) {
        EmptyIterator.getInstance
      } else {
        new client.net.sf.saxon.ce.tree.iter.ListIterator(nodes)
      }
    } else {
      var result: SequenceIterator = null
      val docIndex = docIndexes.get(doc)
      if (docIndex != null) {
        for (indexId ← index.keySet.asInstanceOf[java.lang.Iterable[IndexId]] if indexId.keyName == keyName) {
          val indexObject2 = getIndex(doc, indexId.keyName, indexId.primitiveType)
          if (indexObject2.isInstanceOf[String]) {
            val de = new XPathException("Key definition is circular")
            de.setErrorCode("XTDE0640")
            throw de
          }
          val index2 = indexObject2.asInstanceOf[HashMap]
          if (!index2.isEmpty) {
            value = soughtValue.convert(indexId.primitiveType).asAtomic()
            val nodes = index2.get(getCollationKey(value, indexId.primitiveType, collation, context)).asInstanceOf[ArrayList]
            if (nodes != null) {
              result = if (result == null) new ListIterator(nodes) else new VennIterator(result, new ListIterator(nodes), 
                LocalOrderComparer.getInstance, Token.UNION)
            }
          }
        }
      }
      if (result == null) {
        EmptyIterator.getInstance
      } else {
        result
      }
    }
  }

  /**
   * Save the index associated with a particular key, a particular item type,
   * and a particular document. This
   * needs to be done in such a way that the index is discarded by the garbage collector
   * if the document is discarded. We therefore use a WeakHashMap indexed on the DocumentInfo,
   * which returns HashMap giving the index for each key fingerprint. This index is itself another
   * HashMap.
   * The methods need to be synchronized because several concurrent transformations (which share
   * the same KeyManager) may be creating indexes for the same or different documents at the same
   * time.
   * @param doc the document being indexed
   * @param keyName represents the name of the key definition
   * @param itemType the primitive type of the values being indexed
   * @param index the index being saved
   * @param context the dynamic evaluation context
   */
  private def putIndex(doc: DocumentInfo, 
      keyName: StructuredQName, 
      itemType: AtomicType, 
      index: AnyRef, 
      context: XPathContext): Unit = {
    synchronized {
      if (docIndexes == null) {
        docIndexes = new HashMap[DocumentInfo, HashMap[IndexId, Any]](10)
      }
      val indexRef = docIndexes.get(doc)
      var indexList: HashMap[IndexId, Any] = null
      if (indexRef == null) {
        indexList = new HashMap[IndexId, Any](10)
        val controller = context.getController
        if (controller.getDocumentPool.contains(doc)) {
          context.getController.setUserData(doc, "saxon:key-index-list", indexList)
        } else {
          doc.setUserData("saxon:key-index-list", indexList)
        }
        docIndexes.put(doc, new HashMap[IndexId, Any](indexList))
      } else {
        indexList = indexRef
      }
      indexList.put(new IndexId(keyName, itemType), index)
    }
  }

  /**
   * Get the index associated with a particular key, a particular source document,
   * and a particular primitive item type
   * @param doc the document whose index is required
   * @param keyName the name of the key definition
   * @param itemType the primitive item type of the values being indexed
   * @return either an index (as a HashMap), or the String "under construction", or null
   */
  private def getIndex(doc: DocumentInfo, keyName: StructuredQName, itemType: AtomicType): AnyRef = {
    synchronized {
      if (docIndexes == null) {
        docIndexes = new HashMap[DocumentInfo, HashMap[IndexId, Any]](10)
      }
      val docIndex = docIndexes.get(doc)
      if (docIndex == null) {
        return null
      }
      val id = new IndexId(keyName, itemType)
      docIndex.get(id)
    }
  }
}
