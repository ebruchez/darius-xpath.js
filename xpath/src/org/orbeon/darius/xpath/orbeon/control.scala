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

import java.util.Date

import org.orbeon.darius.xpath.lib.ErrorListener
import org.orbeon.darius.xpath.om.DocumentPool
import org.orbeon.darius.xpath.trans.DecimalFormatManager
import org.orbeon.darius.xpath.value.DateTimeValue

class Controller(config: Configuration, executable: Executable) {

  def this(config: Configuration) =
    this(config, new Executable)

  def getExecutable: Executable = executable
  def newXPathContext() = ???
  def getConfiguration = config
  def getCurrentDateTime: DateTimeValue =
    DateTimeValue.fromJavaDate(new Date())

  def getDocumentPool: DocumentPool = getConfiguration.getDocumentPool

  private var _errorListener: ErrorListener = _

  def getErrorListener: ErrorListener = _errorListener
  def setErrorListener(errorListener: ErrorListener) = _errorListener = errorListener
}

class Executable {
  lazy val getDecimalFormatManager: DecimalFormatManager = new DecimalFormatManager()
}

