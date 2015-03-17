package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.trans.XPathException
import ItemMappingIterator._
//remove if not needed
import scala.collection.JavaConversions._

object ItemMappingIterator {

  /**
   * The mapping function can throw an EarlyExitException to indicate that no more items will be
   * returned; processing of the input sequence can cease at this point.
   */
  class EarlyExitException extends XPathException
}

/**
 * ItemMappingIterator applies a mapping function to each item in a sequence.
 * The mapping function either returns a single item, or null (representing an
 * empty sequence).
 * <p/>
 * This is a specialization of the more general MappingIterator class, for use
 * in cases where a single input item never maps to a sequence of more than one
 * output item.
 */
class ItemMappingIterator(var base: SequenceIterator, var action: ItemMappingFunction)
    extends SequenceIterator with LastPositionFinder {

  private var oneToOne: Boolean = false

  /**
   * Construct an ItemMappingIterator that will apply a specified ItemMappingFunction to
   * each Item returned by the base iterator.
   *
   * @param base   the base iterator
   * @param action the mapping function to be applied
   * @param oneToOne true if this iterator is one-to-one
   */
  def this(base: SequenceIterator, action: ItemMappingFunction, oneToOne: Boolean) {
    this()
    this.base = base
    this.action = action
    this.oneToOne = oneToOne
  }

  protected def getBaseIterator(): SequenceIterator = base

  protected def getMappingFunction(): ItemMappingFunction = action

  def next(): Item = {
    while (true) {
      val nextSource = base.next()
      if (nextSource == null) {
        return null
      }
      try {
        val curr = action.mapItem(nextSource)
        if (curr != null) {
          curr
        }
      } catch {
        case e: EarlyExitException => null
      }
    }
  }

  def getAnother(): SequenceIterator = {
    val newBase = base.getAnother
    val newAction = if (action.isInstanceOf[StatefulMappingFunction]) action.asInstanceOf[StatefulMappingFunction].getAnother(newBase).asInstanceOf[ItemMappingFunction] else action
    new ItemMappingIterator(newBase, newAction, oneToOne)
  }

  def getLastPosition(): Int = {
    if (base.isInstanceOf[LastPositionFinder] && oneToOne) {
      base.asInstanceOf[LastPositionFinder].getLastPosition
    } else {
      -1
    }
  }
}
