package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.`type`.AtomicType
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.value.AtomicValue
import client.net.sf.saxon.ce.value.NumericValue
import client.net.sf.saxon.ce.value.UntypedAtomicValue
import client.net.sf.saxon.ce.value.DoubleValue
import client.net.sf.saxon.ce.trans.XPathException
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Expression that performs numeric promotion to xs:double
 */
class PromoteToFloat(exp: Expression) extends NumericPromoter(exp) {

  /**
   * Determine the data type of the items returned by the expression, if possible
   * @return a value such as Type.STRING, Type.BOOLEAN, Type.NUMBER, Type.NODE,
   * or Type.ITEM (meaning not known in advance)
   */
  def getItemType(): ItemType = AtomicType.FLOAT

  /**
   * Perform the promotion
   * @param value the numeric or untyped atomic value to be promoted
   * @return the value that results from the promotion
   */
  protected def promote(value: AtomicValue): AtomicValue = {
    if (!(value.isInstanceOf[NumericValue] || value.isInstanceOf[UntypedAtomicValue])) {
      typeError("Cannot promote non-numeric value to xs:float", "XPTY0004")
    }
    if (value.isInstanceOf[DoubleValue]) {
      typeError("Cannot promote from xs:double to xs:float", "XPTY0004")
    }
    value.convert(AtomicType.FLOAT).asAtomic()
  }
}
