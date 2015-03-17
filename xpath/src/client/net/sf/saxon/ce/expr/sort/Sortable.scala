package client.net.sf.saxon.ce.expr.sort

//remove if not needed
import scala.collection.JavaConversions._

/**
 * A Sortable is an object that can be sorted using the QuickSort method.
 *
 * @author Michael H. Kay
 *
 */
trait Sortable {

  /**
   * Compare two objects within this Sortable, identified by their position.
   * @return <0 if obj[a]<obj[b], 0 if obj[a]=obj[b], >0 if obj[a]>obj[b]
   */
  def compare(a: Int, b: Int): Int

  /**
   * Swap two objects within this Sortable, identified by their position.
   */
  def swap(a: Int, b: Int): Unit
}
