// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.regex

import java.util.HashMap
import org.orbeon.darius.xpath.`type`.Type
import org.orbeon.darius.xpath.expr.z._
import org.orbeon.darius.xpath.om.{Axis, DocumentInfo, NameChecker, NodeInfo}
import org.orbeon.darius.xpath.orbeon.Configuration
import org.orbeon.darius.xpath.pattern.{NameTest, NodeKindTest}
import org.orbeon.darius.xpath.tree.util.Navigator

import scala.collection.JavaConversions._
import scala.util.control.Breaks

object Categories {

  private var CATEGORIES: HashMap[String, IntPredicate] = null

  def build(): Unit = {
    CATEGORIES = new HashMap[String, IntPredicate](30)
    val config = new Configuration()
    var doc: DocumentInfo = null
    doc = config.buildDocument("categories.xml")
    val iter = doc.iterateAxis(Axis.DESCENDANT, new NameTest(Type.ELEMENT, "", "cat"))
    import Breaks._
    breakable {
      while (true) {
        val item = iter.next().asInstanceOf[NodeInfo]
        if (item == null) {
          break()
        }
        val cat = Navigator.getAttributeValue(item, "", "name")
        val irs = new IntRangeSet()
        val iter2 = item.iterateAxis(Axis.CHILD, NodeKindTest.ELEMENT)
        while (true) {
          val r = iter2.next().asInstanceOf[NodeInfo]
          if (r == null) {
            break()
          }
          val from = Navigator.getAttributeValue(r, "", "f")
          val to = Navigator.getAttributeValue(r, "", "t")
          irs.addRange(Integer.parseInt(from, 16), Integer.parseInt(to, 16))
        }
        CATEGORIES.put(cat, new IntSetPredicate(irs))
      }
    }
    val c = "CLMNPSZ"
    for (i ← 0 until c.length) {
      val ch = c.charAt(i)
      var ip: IntPredicate = null
      for ((key, value) ← CATEGORIES if key.charAt(0) == ch) {
        ip = if (ip == null) value else new IntUnionPredicate(ip, value)
      }
      CATEGORIES.put(ch + "", ip)
    }
  }

  val ESCAPE_s = new IntSetPredicate(IntHashSet.fromArray(Array(9, 10, 13, 32)))

  val ESCAPE_S = new IntComplementPredicate(ESCAPE_s)

  val ESCAPE_i = new IntPredicate() {

    def matches(value: Int): Boolean = {
      NameChecker.isNCNameStartChar(value) || value == ':'
    }
  }

  val ESCAPE_I = new IntPredicate() {

    def matches(value: Int): Boolean = {
      !(NameChecker.isNCNameStartChar(value) || value == ':')
    }
  }

  val ESCAPE_c = new IntPredicate() {

    def matches(value: Int): Boolean = {
      NameChecker.isNCNameChar(value) || value == ':'
    }
  }

  val ESCAPE_C = new IntPredicate() {

    def matches(value: Int): Boolean = {
      !(NameChecker.isNCNameChar(value) || value == ':')
    }
  }

  val ESCAPE_d = getCategory("Nd")

  val ESCAPE_D = new IntComplementPredicate(ESCAPE_d)

  var CATEGORY_P: IntPredicate = getCategory("P")

  var CATEGORY_Z: IntPredicate = getCategory("Z")

  var CATEGORY_C: IntPredicate = getCategory("C")

  val ESCAPE_w = new IntPredicate() {

    def matches(value: Int): Boolean = {
      !(CATEGORY_P.matches(value) || CATEGORY_Z.matches(value) || 
        CATEGORY_C.matches(value))
    }
  }

  val ESCAPE_W = new IntComplementPredicate(ESCAPE_w)

  /**
   * Get a predicate to test characters for membership of one of the Unicode
   * character categories
   *
   * @param cat a one-character or two-character category name, for example L or Lu
   * @return a predicate that tests whether a given character belongs to the category
   */
  def getCategory(cat: String): IntPredicate = {
    if (CATEGORIES == null) {
      build()
    }
    CATEGORIES.get(cat)
  }
}
