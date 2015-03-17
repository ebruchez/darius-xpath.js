// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.trans

import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om.StructuredQName
import java.util.HashMap
import DecimalFormatManager._
//remove if not needed
import scala.collection.JavaConversions._

object DecimalFormatManager {

  val DEFAULT_NAME = new StructuredQName("saxon", NamespaceConstant.SAXON, "default-decimal-format")

  private class DecimalFormatInfo {

    var dfs: DecimalSymbols = _

    var precedence: Int = _
  }
}

/**
 * DecimalFormatManager manages the collection of named and unnamed decimal formats, for use by the
 * format-number() function.
 *
 * <p>In XSLT, there is a single set of decimal formats shared by the whole stylesheet. In XQuery 1.1, however,
 * each query module has its own set of decimal formats. The DecimalFormatManager to use is therefore linked
 * from the format-number() call on the expression tree.</p>
 * @author Michael H. Kay
 */
class DecimalFormatManager {

  private var defaultDFS: DecimalSymbols = new DecimalSymbols()

  private var formatTable: HashMap[StructuredQName, DecimalFormatInfo] = new HashMap[StructuredQName, DecimalFormatInfo](10)

  private var usingOriginalDefault: Boolean = true

  /**
   * Register the default decimal-format.
   * Note that it is an error to register the same decimal-format twice, even with different
   * precedence
   */
  def setDefaultDecimalFormat(dfs: DecimalSymbols, precedence: Int) {
    if (!usingOriginalDefault) {
      if (dfs != defaultDFS) {
        val err = new XPathException("There are two conflicting definitions of the default decimal format")
        err.setErrorCode("XTSE1290")
        err.setIsStaticError(true)
        throw err
      }
    }
    defaultDFS = dfs
    usingOriginalDefault = false
    setNamedDecimalFormat(DEFAULT_NAME, dfs, precedence)
  }

  /**
   * Method called at the end of stylesheet compilation to fix up any format-number() calls
   * to the "default default" decimal format
   */
  def fixupDefaultDefault() {
    if (usingOriginalDefault) {
      setNamedDecimalFormat(DEFAULT_NAME, defaultDFS, -1000)
    }
  }

  /**
   * Get the default decimal-format.
   */
  def getDefaultDecimalFormat(): DecimalSymbols = defaultDFS

  /**
   * Set a named decimal format.
   * Note that it is an error to register the same decimal-format twice, unless the values are
   * equal, or unless there is another of higher precedence. This method assumes that decimal-formats
   * are registered in order of decreasing precedence
   * @param qName the name of the decimal format
   */
  def setNamedDecimalFormat(qName: StructuredQName, dfs: DecimalSymbols, precedence: Int) {
    val o = formatTable.get(qName)
    if (o != null) {
      val info = o.asInstanceOf[DecimalFormatInfo]
      val old = info.dfs
      val oldPrecedence = info.precedence
      if (precedence < oldPrecedence) {
        return
      }
      if (precedence == oldPrecedence && dfs != old) {
        val err = new XPathException("There are two conflicting definitions of the named decimal-format")
        err.setErrorCode("XTSE1290")
        err.setIsStaticError(true)
        throw err
      }
    }
    val dfi = new DecimalFormatInfo()
    dfi.dfs = dfs
    dfi.precedence = precedence
    formatTable.put(qName, dfi)
  }

  /**
   * Get a named decimal-format registered using setNamedDecimalFormat
   * @param qName The  name of the decimal format
   * @return the DecimalFormatSymbols object corresponding to the named locale, if any
   * or null if not set.
   */
  def getNamedDecimalFormat(qName: StructuredQName): DecimalSymbols = {
    val dfi = formatTable.get(qName).asInstanceOf[DecimalFormatInfo]
    if (dfi == null) {
      return null
    }
    dfi.dfs
  }
}
