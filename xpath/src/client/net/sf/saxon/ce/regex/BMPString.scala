/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package client.net.sf.saxon.ce.regex

/**
 * Encapsulates String as CharacterIterator.
 * <p/>
 * Modified by Saxonica (MHK) to encapsulate any CharSequence, provided the CharSequence contains
 * no surrogate pairs.
 *
 * @author <a href="mailto:ales.novak@netbeans.com">Ales Novak</a>
 * @version CVS $Id: StringCharacterIterator.java 518156 2007-03-14 14:31:26Z vgritsenko $
 */
class BMPString(val src: CharSequence) extends UnicodeString {

  def substring(beginIndex: Int, endIndex: Int): UnicodeString = {
    new BMPString(src.subSequence(beginIndex, endIndex))
  }

  def charAt(pos: Int): Int = src.charAt(pos)

  def indexOf(search: Int, pos: Int): Int = {
    if (search > 65535) {
      -1
    } else {
      (pos until src.length).find(src.charAt(_) == search.toChar)
        .getOrElse(-1)
    }
  }

  def length(): Int = src.length

  def isEnd(pos: Int): Boolean = (pos >= src.length)

  override def toString(): String = src.toString

  def getCharSequence(): CharSequence = src
}
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.