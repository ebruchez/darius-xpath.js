/**
 * Copyright 2015 Orbeon, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.orbeon.darius.xpath.orbeon

import org.orbeon.darius.xpath.demo.XPathProcessor
import org.orbeon.darius.xpath.om.Item
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.value
import org.orbeon.darius.xpath.value.BooleanValue
import utest._

object XPathTest extends TestSuite {

  import XPathProcessor._

  val GlobalConfiguration = new Configuration

  def compileAndRun(expr: String): List[Item] =
    compileExpression(expr, GlobalConfiguration).flatMap(runExpression(_, null)).get

  def compileAndRunToStrings(expr: String) =
    compileAndRun(expr) map (_.getStringValue)

  def newInteger(i: Int) =
    new value.IntegerValue(i)

  def newDecimal(s: String) =
    new value.DecimalValue(new java.math.BigDecimal(s))

  def newDouble(d: Double) =
    new  value.DoubleValue(d)

  def toStringList(l: List[Item]) = l map (_.getStringValue)

  def isTrue(l: List[Item]) = l match {
    case List(BooleanValue.TRUE) ⇒ true
    case _                       ⇒ false
  }

  override def tests = TestSuite {
    'MinFunctionOnInteger {
      assert(List(newInteger(42)) == compileAndRun("""min((42, 100))"""))
    }
    'MinFunctionOnString {
      assert(List("aba") == compileAndRunToStrings("""min(("abd", "aba", "abb"))"""))
    }
    'MinFunctionOnNumbers {
      assert(List(newDecimal("42.2")) == compileAndRun("""min((44, 42.2, 43E0))"""))
    }
    'PredicateOptimization {
      assert(List(newDouble(7)) == compileAndRun("""(3, 7)[2]"""))
    }
    'EmptySequence {
      assert(Nil == compileAndRun("""()"""))
    }
    'Matches {

      val multiline =
        """
          |Kaum hat dies der Hahn gesehen,
          |Fängt er auch schon an zu krähen:
          |«Kikeriki! Kikikerikih!!»
          |Tak, tak, tak! - da kommen sie.
        """.stripMargin

      val expected = List(
        true  → """matches("abracadabra", "bra")""",
        true  → """matches("abracadabra", "^a.*a$")""",
        false → """matches("abracadabra", "^bra")"""
//        false → s"""matches($multiline, "Kaum.*krähen")"""
      )

      for ((result, expr) ← expected)
        assert(result == isTrue(compileAndRun(expr)))
    }
    'Tokenize {

      val expected = List(
        List("", "r", "c", "d", "r", "")              → """tokenize("abracadabra", "(ab)|(a)")""",
        List("The", "cat", "sat", "on", "the", "mat") → """tokenize("The cat sat on the mat", "\s+")""",
        List("1", "15", "24", "50")                   → """tokenize("1, 15, 24, 50", ",\s*")""",
        List("1", "15", "", "24", "50", "")           → """tokenize("1,15,,24,50,", ",")""",
        List("Some unparsed", "HTML", "text")         → """tokenize("Some unparsed <br> HTML <BR> text", "\s*<br>\s*", "i")"""
      )

      for ((result, expr) ← expected)
        assert(result == toStringList(compileAndRun(expr)))
    }
    'TokenizeException {
      intercept[XPathException] {
        compileAndRun("""tokenize("abba", ".?")""")
      }
    }
    'Replace {
      val expected = List(
        "a*cada*"         → """replace("abracadabra", "bra", "*")""",
        "*"               → """replace("abracadabra", "a.*a", "*")""",
        "*c*bra"          → """replace("abracadabra", "a.*?a", "*")""",
        "brcdbr"          → """replace("abracadabra", "a", "")""",
        "b"               → """replace("AAAA", "A+", "b")""",
        "bbbb"            → """replace("AAAA", "A+?", "b")""",
        "carted"          → """replace("darted", "^(.*?)d(.*)$", "$1c$2")""",
        "abbraccaddabbra" → """replace("abracadabra", "a(.)", "a$1$1")""",
        "[1=ab][2=]cd"    → """replace("abcd", "(ab)|(a)", "[1=$1][2=$2]")"""
      )

      for ((result, expr) ← expected)
        assert(List(result) == toStringList(compileAndRun(expr)))
    }
    'ReplaceException {
      intercept[XPathException] {
        compileAndRun("""replace("abracadabra", ".*?", "$1")""")
      }
    }
  }
}
