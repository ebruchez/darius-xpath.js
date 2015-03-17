// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.value

import client.net.sf.saxon.ce.`type`.{AtomicType, ConversionResult, StringToDouble, ValidationFailure}
import client.net.sf.saxon.ce.lib.StringCollator
import client.net.sf.saxon.ce.trans.XPathException

/**
 * An Untyped Atomic value. This inherits from StringValue for implementation convenience, even
 * though an untypedAtomic value is not a String in the data model type hierarchy.
 */
class UntypedAtomicValue(_value: CharSequence) extends StringValue {

  var doubleValue: DoubleValue = null

  this.value = if (_value == null) "" else _value

  /**
   * Determine the primitive type of the value. This delivers the same answer as
   * getItemType().getPrimitiveItemType(). The primitive types are
   * the 19 primitive types of XML Schema, plus xs:integer, xs:dayTimeDuration and xs:yearMonthDuration,
   * and xs:untypedAtomic. For external objects, the result is AnyAtomicType.
   */
  override def getItemType(): AtomicType = AtomicType.UNTYPED_ATOMIC

  /**
   * Convert a value to another primitive data type, with control over how validation is
   * handled.
   *
   * @param requiredType type code of the required atomic type. This must not be a namespace-sensitive type.
   * @return the result of the conversion, if successful. If unsuccessful, the value returned
   * will be a ValidationErrorValue. The caller must check for this condition. No exception is thrown, instead
   * the exception will be encapsulated within the ErrorValue.
   */
  override def convert(requiredType: AtomicType): ConversionResult = {
    if (requiredType == AtomicType.STRING) {
      if (value.length == 0) {
        StringValue.EMPTY_STRING
      } else {
        new StringValue(value)
      }
    } else if (requiredType == AtomicType.UNTYPED_ATOMIC) {
      this
    } else if (requiredType == AtomicType.DOUBLE || requiredType == AtomicType.NUMERIC) {
      try {
        toDouble()
      } catch {
        case e: XPathException => new ValidationFailure(e.getMessage)
      }
    } else {
      super.convert(requiredType)
    }
  }

  /**
   * Convert the value to a double, returning a DoubleValue
   */
  private def toDouble(): AtomicValue = {
    if (doubleValue == null) {
      val d = StringToDouble.stringToNumber(value)
      doubleValue = new DoubleValue(d)
    }
    doubleValue
  }

  /**
   * Compare an untypedAtomic value with another value, using a given collator to perform
   * any string comparisons. This works by converting the untypedAtomic value to the type
   * of the other operand, which is the correct behavior for operators like "=" and "!=",
   * but not for "eq" and "ne": in the latter case, the untypedAtomic value is converted
   * to a string and this method is therefore not used.
   * @return -1 if the this value is less than the other, 0 if they are equal, +1 if this
   * value is greater.
   * @throws ClassCastException if the value cannot be cast to the type of the other operand
   */
  def compareTo(other: AtomicValue, collator: StringCollator): Int = {
    if (other.isInstanceOf[NumericValue]) {
      if (doubleValue == null) {
        doubleValue = convert(AtomicType.DOUBLE).asAtomic().asInstanceOf[DoubleValue]
      }
      doubleValue.compareTo(other)
    } else if (other.isInstanceOf[StringValue]) {
      collator.compareStrings(getStringValue, other.getStringValue)
    } else {
      val result = convert(other.getItemType)
      if (result.isInstanceOf[ValidationFailure]) {
        throw new ClassCastException("Cannot convert untyped atomic value '" + getStringValue + 
          "' to type " + 
          other.getItemType)
      }
      result.asInstanceOf[Comparable[AnyRef]].compareTo(other)
    }
  }
}
