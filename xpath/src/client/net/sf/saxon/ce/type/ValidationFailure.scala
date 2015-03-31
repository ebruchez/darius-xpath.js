// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.`type`

import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.value.AtomicValue

/**
 * This exception indicates a failure when validating an instance against a type
 * defined in a schema.
 *
 * <p>This class holds the same information as a ValidationException, except that it is not an exception,
 * and does not carry system overheads such as a stack trace. It is used because operations such as "castable",
 * and validation of values in a union, cause validation failures on a success path and it is costly to throw,
 * or even to create, exception objects on a success path.</p>
 */
class ValidationFailure(val message: String, private var errorCode: StructuredQName) extends ConversionResult {

  def this(message: String) =
    this(message, null: StructuredQName)

  def this(message: String, errorCode: String) =
    this(message, new StructuredQName("err", NamespaceConstant.ERR, errorCode))

  def getMessage = message

  /**
   * Returns the String representation of this Exception
   * @return the String representation of this Exception
   *
   */
  override def toString(): String = {
    "ValidationException" + (if (message == null) "" else ": " + message)
  }

  def setErrorCode(errorCode: String): Unit = {
    this.errorCode = new StructuredQName("err", NamespaceConstant.ERR, errorCode)
  }

  def getErrorCodeQName: StructuredQName = errorCode

  /**
   * Calling this method on a ConversionResult returns the AtomicValue that results
   * from the conversion if the conversion was successful, and throws a ValidationException
   * explaining the conversion error otherwise.
   * <p/>
   * <p>Use this method if you are calling a conversion method that returns a ConversionResult,
   * and if you want to throw an exception if the conversion fails.</p>
   *
   * @return the atomic value that results from the conversion if the conversion was successful
   * @throws XPathException
   *          if the conversion was not successful
   */
  def asAtomic(): AtomicValue = {
    val ve = new XPathException(message)
    if (errorCode == null) {
      ve.setErrorCode("FORG0001")
    } else {
      ve.setErrorCodeQName(errorCode)
    }
    throw ve
  }
}
