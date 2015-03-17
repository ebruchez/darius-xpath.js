package client.net.sf.saxon.ce.lib

import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Simplified variant of the JAXP ErrorListener interface
 */
trait ErrorListener {

  def error(err: XPathException): Unit
}
