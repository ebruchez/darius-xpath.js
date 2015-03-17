package client.net.sf.saxon.ce.om

import client.net.sf.saxon.ce.lib.NamespaceConstant
import NamespaceBinding._
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

object NamespaceBinding {

  val XML = new NamespaceBinding("xml", NamespaceConstant.XML)

  val DEFAULT_UNDECLARATION = new NamespaceBinding("", "")

  val EMPTY_ARRAY = new Array[NamespaceBinding](0)
}

/**
 * Represents the binding of a prefix to a URI. Also, in some contexts, represents an unbinding, by
 * virtue of the URI being set to a zero length string.
 * @since 9.4
 */
class NamespaceBinding(@BeanProperty var prefix: String, var uri: String) {

  if (prefix == null || uri == null) {
    throw new NullPointerException()
  }

  /**
   * Get the URI part of the binding
   * @return the URI. Never null. The zero-length string indicates an unbinding of the prefix. For the
   * default namespace (prefix="") this indicates that the prefix refers to names in no namespace; for other
   * prefixes, it indicates that the prefix is not bound to any namespace and therefore cannot be used.
   */
  def getURI(): String = uri

  /**
   * Test if this namespace binding is the same as another
   * @param obj the comparand
   * @return true if the comparand is a Namespace binding of the same prefix to the same URI
   */
  override def equals(obj: Any): Boolean = {
    obj.isInstanceOf[NamespaceBinding] && 
      prefix == obj.asInstanceOf[NamespaceBinding].getPrefix && 
      uri == obj.asInstanceOf[NamespaceBinding].getURI
  }

  override def hashCode(): Int = prefix.hashCode ^ uri.hashCode
}
