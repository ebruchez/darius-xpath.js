package client.net.sf.saxon.ce.om

//remove if not needed
import scala.collection.JavaConversions._

object CopyOptions {

  val LOCAL_NAMESPACES = 1

  val ALL_NAMESPACES = 2

  val SOME_NAMESPACES = LOCAL_NAMESPACES | ALL_NAMESPACES

  val TYPE_ANNOTATIONS = 4

  def includes(options: Int, option: Int): Boolean = (options & option) == option
}
