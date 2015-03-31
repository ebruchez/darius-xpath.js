// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.functions

import java.math.BigDecimal

import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.functions.FormatNumber._
import client.net.sf.saxon.ce.om.{Item, StructuredQName}
import client.net.sf.saxon.ce.orbeon.ArrayList
import client.net.sf.saxon.ce.trans.{DecimalSymbols, XPathException}
import client.net.sf.saxon.ce.tree.util.FastStringBuffer
import client.net.sf.saxon.ce.value.{StringValue, _}

object FormatNumber {

  /**
   * Analyze a picture string into two sub-pictures.
   *
   * @param picture the picture as written (possibly two subpictures separated by a semicolon)
   * @param dfs     the decimal format symbols
   * @return an array of two sub-pictures, the positive and the negative sub-pictures respectively.
   *         If there is only one sub-picture, the second one is null.
   */
  private def getSubPictures(picture: String, dfs: DecimalSymbols): Array[SubPicture] = {
    val picture4 = StringValue.expand(picture)
    val pics = new Array[SubPicture](2)
    if (picture4.length == 0) {
      val err = new XPathException("format-number() picture is zero-length")
      err.setErrorCode("XTDE1310")
      throw err
    }
    var sep = -1
    for (c ← 0 until picture4.length if picture4(c) == dfs.patternSeparator) {
      if (c == 0) {
        grumble("first subpicture is zero-length")
      } else if (sep >= 0) {
        grumble("more than one pattern separator")
      } else if (sep == picture4.length - 1) {
        grumble("second subpicture is zero-length")
      }
      sep = c
    }
    if (sep < 0) {
      pics(0) = new SubPicture(picture4, dfs)
      pics(1) = null
    } else {
      val pic0 = new Array[Int](sep)
      System.arraycopy(picture4, 0, pic0, 0, sep)
      val pic1 = new Array[Int](picture4.length - sep - 1)
      System.arraycopy(picture4, sep + 1, pic1, 0, picture4.length - sep - 1)
      pics(0) = new SubPicture(pic0, dfs)
      pics(1) = new SubPicture(pic1, dfs)
    }
    pics
  }

  /**
   * Format a number, given the two subpictures and the decimal format symbols
   *
   * @param number      the number to be formatted
   * @param subPictures the negative and positive subPictures
   * @param dfs         the decimal format symbols to be used
   * @return the formatted number
   */
  private def formatNumber(number: NumericValue, subPictures: Array[SubPicture], dfs: DecimalSymbols): CharSequence = {
    var absN = number
    var pic: SubPicture = null
    var minusSign = ""
    if (number.signum() < 0) {
      absN = number.negate()
      if (subPictures(1) == null) {
        pic = subPictures(0)
        minusSign = "" + unicodeChar(dfs.minusSign)
      } else {
        pic = subPictures(1)
      }
    } else {
      pic = subPictures(0)
    }
    pic.format(absN, dfs, minusSign)
  }

  private def grumble(s: String): Unit = {
    throw new XPathException("format-number picture: " + s, "XTDE1310")
  }

  /**
   * Convert a double to a BigDecimal. In general there will be several BigDecimal values that
   * are equal to the supplied value, and the one we want to choose is the one with fewest non-zero
   * digits. The algorithm used is rather pragmatic: look for a string of zeroes or nines, try rounding
   * the number down or up as approriate, then convert the adjusted value to a double to see if it's
   * equal to the original: if not, use the original value unchanged.
   *
   * @param value     the double to be converted
   * @param precision 2 for a double, 1 for a float
   * @return the result of conversion to a double
   */
  def adjustToDecimal(value: Double, precision: Int): BigDecimal = {
    val zeros = if (precision == 1) "00000" else "000000000"
    val nines = if (precision == 1) "99999" else "999999999"
    val initial = new BigDecimal(value)
    var trial: BigDecimal = null
    val fsb = new FastStringBuffer(FastStringBuffer.TINY)
    DecimalValue.decimalToString(initial, fsb)
    val s = fsb.toString
    val start = if (s.charAt(0) == '-') 1 else 0
    val p = s.indexOf(".")
    var i = s.lastIndexOf(zeros)
    if (i > 0) {
      if (p < 0 || i < p) {
        val sb = new FastStringBuffer(s.length)
        sb.append(s.substring(0, i))
        for (n ← i until s.length) {
          sb.append(if (s.charAt(n) == '.') '.' else '0')
        }
        trial = new BigDecimal(sb.toString)
      } else {
        trial = new BigDecimal(s.substring(0, i))
      }
    } else {
      i = s.indexOf(nines)
      if (i >= 0) {
        if (i == start) {
          val sb = new FastStringBuffer(s.length + 1)
          if (start == 1) {
            sb.append('-')
          }
          sb.append('1')
          for (n ← start until s.length) {
            sb.append(if (s.charAt(n) == '.') '.' else '0')
          }
          trial = new BigDecimal(sb.toString)
        } else {
          while (i >= 0 && (s.charAt(i) == '9' || s.charAt(i) == '.')) {
            i -= 1
          }
          if (i < 0 || s.charAt(i) == '-') {
            return initial
          } else if (p < 0 || i < p) {
            val sb = new FastStringBuffer(s.length)
            sb.append(s.substring(0, i))
            sb.append((s.charAt(i).toInt + 1).toChar)
            for (n ← i until s.length) {
              sb.append(if (s.charAt(n) == '.') '.' else '0')
            }
            trial = new BigDecimal(sb.toString)
          } else {
            val s2 = s.substring(0, i) + (s.charAt(i).toInt + 1).toChar
            trial = new BigDecimal(s2)
          }
        }
      }
    }
    if (trial != null &&
      (if (precision == 1) trial.floatValue() == value else trial.doubleValue() == value)) {
      trial
    } else {
      initial
    }
  }

  /**
   * Inner class to represent one sub-picture (the negative or positive subpicture)
   */
  private class SubPicture(pic: Array[Int], dfs: DecimalSymbols) {

    var minWholePartSize: Int = 0

    var maxWholePartSize: Int = 0

    var minFractionPartSize: Int = 0

    var maxFractionPartSize: Int = 0

    var isPercent: Boolean = false

    var isPerMille: Boolean = false

    var prefix: String = ""

    var suffix: String = ""

    var wholePartGroupingPositions: Array[Int] = null

    var fractionalPartGroupingPositions: Array[Int] = null

    val percentSign = dfs.percent

    val perMilleSign = dfs.permill

    val decimalSeparator = dfs.decimalSeparator

    val groupingSeparator = dfs.groupingSeparator

    val digitSign = dfs.digit

    val zeroDigit = dfs.zeroDigit

    var wholePartPositions: ArrayList[Int] = null

    var fractionalPartPositions: ArrayList[Int] = null

    var foundDigit = false

    var foundDecimalSeparator = false

    for (c ← pic if c == digitSign || c == zeroDigit) {
      foundDigit = true
      //break
    }

    if (!foundDigit) {
      grumble("subpicture contains no digit or zero-digit sign")
    }

    var phase = 0

    for (c ← pic) {
      if (c == percentSign || c == perMilleSign) {
        if (isPercent || isPerMille) {
          grumble("Cannot have more than one percent or per-mille character in a sub-picture")
        }
        isPercent = c == percentSign
        isPerMille = c == perMilleSign
        phase match {
          case 0 ⇒ prefix += unicodeChar(c)
          case 1 | 2 | 3 | 4 | 5 ⇒
            phase = 5
            suffix += unicodeChar(c)

        }
      } else if (c == digitSign) phase match {
        case 0 | 1 ⇒
          phase = 1
          maxWholePartSize += 1

        case 2 ⇒ grumble("Digit sign must not appear after a zero-digit sign in the integer part of a sub-picture")
        case 3 | 4 ⇒
          phase = 4
          maxFractionPartSize += 1

        case 5 ⇒ grumble("Passive character must not appear between active characters in a sub-picture")
      } else if (c == zeroDigit) phase match {
        case 0 | 1 | 2 ⇒
          phase = 2
          minWholePartSize += 1
          maxWholePartSize += 1

        case 3 ⇒
          minFractionPartSize += 1
          maxFractionPartSize += 1

        case 4 ⇒ grumble("Zero digit sign must not appear after a digit sign in the fractional part of a sub-picture")
        case 5 ⇒ grumble("Passive character must not appear between active characters in a sub-picture")
      } else if (c == decimalSeparator) phase match {
        case 0 | 1 | 2 ⇒
          phase = 3
          foundDecimalSeparator = true

        case 3 | 4 | 5 ⇒ if (foundDecimalSeparator) {
          grumble("There must only be one decimal separator in a sub-picture")
        } else {
          grumble("Decimal separator cannot come after a character in the suffix")
        }
      } else if (c == groupingSeparator) phase match {
        case 0 | 1 | 2 ⇒
          if (wholePartPositions == null) {
            wholePartPositions = new ArrayList[Int](3)
          }
          wholePartPositions.add(maxWholePartSize)

        case 3 | 4 ⇒
          if (maxFractionPartSize == 0) {
            grumble("Grouping separator cannot be adjacent to decimal separator")
          }
          if (fractionalPartPositions == null) {
            fractionalPartPositions = new ArrayList[Int](3)
          }
          fractionalPartPositions.add(maxFractionPartSize)

        case 5 ⇒ grumble("Grouping separator found in suffix of sub-picture")
      } else phase match {
        case 0 ⇒ prefix += unicodeChar(c)
        case 1 | 2 | 3 | 4 | 5 ⇒
          phase = 5
          suffix += unicodeChar(c)

      }
    }

    if (minWholePartSize == 0 && !foundDecimalSeparator) {
      minWholePartSize = 1
    }

    if (wholePartPositions != null) {
      val n = wholePartPositions.size
      wholePartGroupingPositions = new Array[Int](n)
      for (i ← 0 until n) {
        wholePartGroupingPositions(i) = maxWholePartSize -
          wholePartPositions.get(n - i - 1).asInstanceOf[java.lang.Integer]
      }
      if (n > 1) {
        var regular = true
        val first = wholePartGroupingPositions(0)
        for (i ← 1 until n if wholePartGroupingPositions(i) != i * first) {
          regular = false
          //break
        }
        if (regular) {
          wholePartGroupingPositions = new Array[Int](1)
          wholePartGroupingPositions(0) = first
        }
      }
      if (wholePartGroupingPositions(0) == 0) {
        grumble("Cannot have a grouping separator adjacent to the decimal separator")
      }
    }

    if (fractionalPartPositions != null) {
      val n = fractionalPartPositions.size
      fractionalPartGroupingPositions = new Array[Int](n)
      for (i ← 0 until n) {
        fractionalPartGroupingPositions(i) = fractionalPartPositions.get(i).intValue()
      }
    }

    /**
     * Format a number using this sub-picture
     *
     * @param _value     the absolute value of the number to be formatted
     * @param dfs       the decimal format symbols to be used
     * @param minusSign the representation of a minus sign to be used
     * @return the formatted number
     */
    def format(_value: NumericValue, dfs: DecimalSymbols, minusSign: String): CharSequence = {
      var value = _value
      if (value.isNaN) {
        return dfs.NaN
      }
      if ((value.isInstanceOf[DoubleValue] || value.isInstanceOf[FloatValue]) && value.getDoubleValue.isInfinite) {
        return minusSign + prefix + dfs.infinity + suffix
      }
      var multiplier = 1
      if (isPercent) {
        multiplier = 100
      } else if (isPerMille) {
        multiplier = 1000
      }
      if (multiplier != 1) {
        try {
          value = ArithmeticExpression.compute(value, Token.MULT, new IntegerValue(multiplier), null).asInstanceOf[NumericValue]
        } catch {
          case e: XPathException ⇒ value = new DoubleValue(value.getDoubleValue * multiplier)
        }
      }
      val sb = new FastStringBuffer(FastStringBuffer.TINY)
      if (value.isInstanceOf[DoubleValue] || value.isInstanceOf[FloatValue]) {
        val dec = adjustToDecimal(value.getDoubleValue, 2)
        formatDecimal(dec, sb)
      } else if (value.isInstanceOf[IntegerValue]) {
        formatInteger(value, sb)
      } else if (value.isInstanceOf[DecimalValue]) {
        formatDecimal(value.asInstanceOf[DecimalValue].getDecimalValue, sb)
      }
      var ib = StringValue.expand(sb)
      var ibused = ib.length
      var point = sb.indexOf('.')
      if (point == -1) {
        point = sb.length
      } else {
        ib(point) = dfs.decimalSeparator
        if (maxFractionPartSize == 0) {
          ibused -= 1
        }
      }
      if (dfs.zeroDigit != '0') {
        val newZero = dfs.zeroDigit
        for (i ← 0 until ibused) {
          val c = ib(i)
          if (c >= '0' && c <= '9') {
            ib(i) = c - '0' + newZero
          }
        }
      }
      if (wholePartGroupingPositions != null) {
        if (wholePartGroupingPositions.length == 1) {
          val g = wholePartGroupingPositions(0)
          var p = point - g
          while (p > 0) {
            ib = insert(ib, ibused, dfs.groupingSeparator, p)
            ibused += 1
            p -= g
          }
        } else {
          for (wholePartGroupingPosition ← wholePartGroupingPositions) {
            val p = point - wholePartGroupingPosition
            if (p > 0) {
              ib = insert(ib, ibused, dfs.groupingSeparator, p)
              ibused += 1
            }
          }
        }
      }
      if (fractionalPartGroupingPositions != null) {
        for (i ← 0 until fractionalPartGroupingPositions.length) {
          val p = point + 1 + fractionalPartGroupingPositions(i) + i
          if (p < ibused - 1) {
            ib = insert(ib, ibused, dfs.groupingSeparator, p)
            ibused += 1
          } else {
            //break
          }
        }
      }
      val res = new FastStringBuffer(prefix.length + minusSign.length + suffix.length + ibused)
      res.append(minusSign)
      res.append(prefix)
      res.append(StringValue.contract(ib, ibused))
      res.append(suffix)
      res
    }

    /**
     * Format a number supplied as a decimal
     *
     * @param _dval the decimal value
     * @param fsb  the FastStringBuffer to contain the result
     */
    private def formatDecimal(_dval: BigDecimal, fsb: FastStringBuffer): Unit = {
      val dval = _dval.setScale(maxFractionPartSize, BigDecimal.ROUND_HALF_EVEN)
      DecimalValue.decimalToString(dval, fsb)
      val point = fsb.indexOf('.')
      var intDigits: Int = 0
      if (point >= 0) {
        var zz = maxFractionPartSize - minFractionPartSize
        while (zz > 0) {
          if (fsb.charAt(fsb.length - 1) == '0') {
            fsb.setLength(fsb.length - 1)
            zz -= 1
          } else {
            //break
          }
        }
        intDigits = point
        if (fsb.charAt(fsb.length - 1) == '.') {
          fsb.setLength(fsb.length - 1)
        }
      } else {
        intDigits = fsb.length
        if (minFractionPartSize > 0) {
          fsb.append('.')
          for (i ← 0 until minFractionPartSize) {
            fsb.append('0')
          }
        }
      }
      if (minWholePartSize == 0 && intDigits == 1 && fsb.charAt(0) == '0') {
        fsb.removeCharAt(0)
      } else {
        fsb.prependRepeated('0', minWholePartSize - intDigits)
      }
    }

    /**
     * Format a number supplied as a integer
     *
     * @param value the integer value
     * @param fsb   the FastStringBuffer to contain the result
     */
    private def formatInteger(value: NumericValue, fsb: FastStringBuffer): Unit = {
      fsb.append(value.getStringValue)
      val leadingZeroes = minWholePartSize - fsb.length
      fsb.prependRepeated('0', leadingZeroes)
      if (minFractionPartSize != 0) {
        fsb.append('.')
        for (i ← 0 until minFractionPartSize) {
          fsb.append('0')
        }
      }
    }
  }

  /**
   * Convert a Unicode character (possibly >65536) to a String, using a surrogate pair if necessary
   *
   * @param ch the Unicode codepoint value
   * @return a string representing the Unicode codepoint, either a string of one character or a surrogate pair
   */
  private def unicodeChar(ch: Int): CharSequence = {
    if (ch < 65536) {
      "" + ch.toChar
    } else {
      new FastStringBuffer(2).appendWideChar(ch)
    }
  }

  /**
   * Insert an integer into an array of integers. This may or may not modify the supplied array.
   *
   * @param _array    the initial array
   * @param used     the number of items in the initial array that are used
   * @param value    the integer to be inserted
   * @param position the position of the new integer in the final array
   * @return the new array, with the new integer inserted
   */
  private def insert(_array: Array[Int],
      used: Int,
      value: Int,
      position: Int): Array[Int] = {
    var array = _array
    if (used + 1 > array.length) {
      val a2 = new Array[Int](used + 10)
      System.arraycopy(array, 0, a2, 0, used)
      array = a2
    }
    System.arraycopy(array, position, array, position + 1, used - 1 - position)
    array(position) = value
    array
  }
}

/**
 * XSLT 2.0 implementation of format-number() function - removes the dependence on the JDK.
 */
class FormatNumber extends SystemFunction {

  def newInstance(): FormatNumber = new FormatNumber()

  private var staticContext: StaticContext = _

  override def checkArguments(visitor: ExpressionVisitor): Unit = {
    if (staticContext == null) {
      staticContext = visitor.getStaticContext
    }
  }

  /**
   * preEvaluate: this method suppresses compile-time evaluation by doing nothing.
   * We can't evaluate early because we don't have access to the DecimalFormatManager.
   *
   * @param visitor the expression visitor
   */
  override def preEvaluate(visitor: ExpressionVisitor): Expression = this

  /**
   * Evaluate in a context where a string is wanted
   */
  override def evaluateAsString(context: XPathContext): CharSequence = {
    val numArgs = argument.length
    var av0 = argument(0).evaluateItem(context).asInstanceOf[AtomicValue]
    if (av0 == null) {
      av0 = DoubleValue.NaN
    }
    val number = av0.asInstanceOf[NumericValue]
    var dfs: DecimalSymbols = null
    val dfm = context.getController.getExecutable.getDecimalFormatManager
    if (numArgs == 2) {
      dfs = dfm.getDefaultDecimalFormat
    } else {
      val lexicalName = argument(2).evaluateItem(context).getStringValue
      var qName: StructuredQName = null
      try {
        qName = StructuredQName.fromLexicalQName(lexicalName, "", staticContext.getNamespaceResolver)
      } catch {
        case e: XPathException ⇒ dynamicError("Invalid decimal format name. " + e.getMessage, "XTDE1280")
      }
      dfs = dfm.getNamedDecimalFormat(qName)
      if (dfs == null) {
        dynamicError("format-number function: decimal-format '" + lexicalName +
          "' is not defined", "XTDE1280")
      }
    }
    val format = argument(1).evaluateItem(context).getStringValue
    val pics = getSubPictures(format, dfs)
    formatNumber(number, pics, dfs)
  }

  /**
   * Evaluate in a general context
   */
  override def evaluateItem(c: XPathContext): Item = new StringValue(evaluateAsString(c))
}
