// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.trace

import java.util.EventListener
import client.net.sf.saxon.ce.Controller
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.lib.TraceListener
import client.net.sf.saxon.ce.om.Item
import TraceEventMulticaster._
//remove if not needed
import scala.collection.JavaConversions._

object TraceEventMulticaster {

  /**
   * Adds trace-listener-a with trace-listener-b and
   * returns the resulting multicast listener.
   *
   * @param a trace-listener-a
   * @param b trace-listener-b
   */
  def add(a: TraceListener, b: TraceListener): TraceListener = {
    addInternal(a, b).asInstanceOf[TraceListener]
  }

  /**
   * Removes the old trace-listener from trace-listener-l and
   * returns the resulting multicast listener.
   *
   * @param l    trace-listener-l
   * @param oldl the trace-listener being removed
   */
  def remove(l: TraceListener, oldl: TraceListener): TraceListener = {
    removeInternal(l, oldl).asInstanceOf[TraceListener]
  }

  /**
   * Returns the resulting multicast listener from adding listener-a
   * and listener-b together.
   * If listener-a is null, it returns listener-b;
   * If listener-b is null, it returns listener-a
   * If neither are null, then it creates and returns
   * a new EventMulticaster instance which chains a with b.
   *
   * @param a event listener-a
   * @param b event listener-b
   */
  protected def addInternal(a: EventListener, b: EventListener): EventListener = {
    if (a == null) {
      return b
    }
    if (b == null) {
      return a
    }
    new TraceEventMulticaster(a, b)
  }

  /**
   * Returns the resulting multicast listener after removing the
   * old listener from listener-l.
   * If listener-l equals the old listener OR listener-l is null,
   * returns null.
   * Else if listener-l is an instance of SaxonEventMulticaster,
   * then it removes the old listener from it.
   * Else, returns listener l.
   *
   * @param l    the listener being removed from
   * @param oldl the listener being removed
   */
  protected def removeInternal(l: EventListener, oldl: EventListener): EventListener = {
    if (l == oldl || l == null) {
      null
    } else if (l.isInstanceOf[TraceEventMulticaster]) {
      l.asInstanceOf[TraceEventMulticaster].remove(oldl)
    } else {
      l
    }
  }
}

/**
 * A class which implements efficient and thread-safe multi-cast event
 * dispatching for the TraceListener evants.
 */
class TraceEventMulticaster protected (protected val a: EventListener, protected val b: EventListener)
    extends TraceListener {

  protected def remove(oldl: EventListener): EventListener = {
    if (oldl == a) {
      return b
    }
    if (oldl == b) {
      return a
    }
    val a2 = removeInternal(a, oldl)
    val b2 = removeInternal(b, oldl)
    if (a2 == a && b2 == b) {
      return this
    }
    addInternal(a2, b2)
  }

  /**
   * Called at start
   */
  def open(): Unit = {
    a.asInstanceOf[TraceListener].open()
    b.asInstanceOf[TraceListener].open()
  }

  /**
   * Called at end
   */
  def close(): Unit = {
    a.asInstanceOf[TraceListener].close()
    b.asInstanceOf[TraceListener].close()
  }

  /**
   * Called when an element of the stylesheet gets processed
   */
  def enter(element: InstructionInfo, context: XPathContext): Unit = {
    a.asInstanceOf[TraceListener].enter(element, context)
    b.asInstanceOf[TraceListener].enter(element, context)
  }

  /**
   * Called after an element of the stylesheet got processed
   */
  def leave(element: InstructionInfo): Unit = {
    a.asInstanceOf[TraceListener].leave(element)
    b.asInstanceOf[TraceListener].leave(element)
  }

  /**
   * Called when an item becomes current
   */
  def startCurrentItem(item: Item): Unit = {
    a.asInstanceOf[TraceListener].startCurrentItem(item)
    b.asInstanceOf[TraceListener].startCurrentItem(item)
  }

  /**
   * Called when an item ceases to be the current item
   */
  def endCurrentItem(item: Item): Unit = {
    a.asInstanceOf[TraceListener].endCurrentItem(item)
    b.asInstanceOf[TraceListener].endCurrentItem(item)
  }
}
