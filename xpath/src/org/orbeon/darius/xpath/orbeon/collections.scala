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

import java.{util ⇒ ju}

import scala.collection.JavaConverters._
import scala.collection.{immutable ⇒ i}

class Stack[T](var underlying: i.List[T]) {

  def this() = this(Nil)

  def push(t: T): Unit = underlying ::= t

  def pop(): T = {
    val result = underlying.head
    underlying = underlying.tail
    result
  }

  def size: Int = underlying.size

  def get(i: Int): T = underlying(i)

  def iterator(): ju.Iterator[T] = underlying.iterator.asJava

  def toArray(a: Array[T]): Array[T] = {
    require(a.length == underlying.size)
    underlying.copyToArray(a)
    a
  }

  def isEmpty: Boolean = underlying.isEmpty
}

object Util {

  // TODO: This only supports ASCII (see http://www.docjar.com/html/api/java/lang/Character$valueOfCache.java.html)
  // TODO: check from Harmony?
  def isLetter(c: Int): Boolean =
    ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')

  // TODO: This only supports ASCII (see http://www.docjar.com/html/api/java/lang/Character$valueOfCache.java.html)
  // TODO: check from Harmony?
  def isLetterOrDigit(c: Int): Boolean =
    isLetter(c) || (c >= '0' && c <= '9')

  // TODO: check from Harmony?
  def compareStringsCaseInsensitive(a: String, b: String): Int = {
    val aLen = a.length
    val bLen = b.length
    var aIndex = 0
    var bIndex = 0

    while (aIndex < aLen && bIndex < bLen) {
      var aCurrent = a.charAt(aIndex)
      var bCurrent = b.charAt(bIndex)
      if(aCurrent != bCurrent) {
        aCurrent = Character.toUpperCase(aCurrent)
        bCurrent = Character.toUpperCase(bCurrent)
        if (aCurrent != bCurrent) {
          aCurrent = Character.toLowerCase(aCurrent)
          bCurrent = Character.toLowerCase(bCurrent)
          if(aCurrent != bCurrent) {
            return aCurrent - bCurrent
          }
        }
      }

      aIndex += 1
      bIndex += 1
    }

    aLen - bLen
  }
}

trait Logger {
  def warning(s: String): Unit
  def info(s: String): Unit
  def severe(s: String): Unit
}

object Logger {
  def getLogger(s: String): Logger = new Logger {
    def warning(s: String) = println(s"warning: $s")
    def severe(s: String)  = println(s"severe: $s")
    def info(s: String)    = println(s"info: $s")
  }
}