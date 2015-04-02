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
package client.net.sf.saxon.ce.orbeon

import client.net.sf.saxon.ce.value
import utest._

object XPathTest extends TestSuite {

  import XPathProcessor._

  def compileAndRun(expr: String) =
    compileExpression(expr).flatMap(runExpression(_, null)).get

  def compileAndRunToStrings(expr: String) =
    compileAndRun(expr) map (_.getStringValue)

  def newInteger(i: Int) =
    new value.IntegerValue(i)

  def newDecimal(s: String) =
    new value.DecimalValue(new java.math.BigDecimal(s))

  def newDouble(d: Double) =
    new  value.DoubleValue(d)

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
  }
}
