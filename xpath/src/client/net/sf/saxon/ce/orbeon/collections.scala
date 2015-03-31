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

import scala.collection.{immutable ⇒ i, mutable ⇒ m}
import scala.{collection ⇒ sc}

// IDEA: We could use, for Map[String, V], native JS object type. For other type of keys, such as Int,
// we could also uniquely convert the keys to String. There is one use of AtomicType as a key, but that
// too can be reduced to a string which is the name of the type. The same would go for Set[String].
// There is one use of Set[Any] in the code to look at though.
class HashMap[K, V](underlying: m.HashMap[K, V]) extends Map[K, V] {

  def this() = this(new m.HashMap[K, V]())
  def this(i: Int) = this()

  def get(k: K)(implicit ev: Null <:< V): V = underlying.get(k).orNull
  def put(k: K, v: V): Unit = underlying += k → v
  def remove(k: K): Unit = underlying -= k
  def keysIterator(): Iterator[K] = Iterator(underlying.keysIterator)
  def containsKey(k: K): Boolean = underlying.contains(k)
  def containsValue(v: V): Boolean = underlying.exists(_._2 == v)

  def foreach[U](f: ((K, V)) ⇒ U): Unit = underlying.foreach(f)
  def withFilter(p: ((K, V)) ⇒ Boolean): sc.Iterator[(K, V)] = underlying.iterator.filter(p)
}

class HashSet[T](underlying: m.Set[T]) extends Set[T] {

  def this() = this(new m.HashSet[T])
  def this(initialSize: Int) = this(new m.HashSet[T])

  def add(t: T): Boolean = underlying.add(t)
  def contains(t: T) = underlying.contains(t)
  def iterator() = Iterator(underlying.iterator)
}

trait Set[T] {
  def add(t: T): Boolean
  def contains(t: T): Boolean
  def iterator(): Iterator[T]
}

class ArrayList[T](underlying: m.ArrayBuffer[T]) extends List[T] {

  def this(initialSize: Int) = this(new m.ArrayBuffer[T](initialSize))
  def this() = this(16)

  def add(t: T): Unit = underlying += t
  def get(i: Int) = underlying(i)
  def set(i: Int, t: T): Unit = underlying(i) = t
  def size = underlying.size
  def iterator(): Iterator[T] = Iterator(underlying.iterator)
  def isEmpty = underlying.isEmpty
  def contains(t: T) = underlying.contains(t)

  def toArray[U >: T](a: Array[U]): Array[U] = {
    require(a.length == underlying.size) // this is the case for known Saxon callers
    underlying.copyToArray(a)
    a
  }

  def foreach[U](f: (T) ⇒ U) = underlying.foreach(f)
  def filter(p: T ⇒ Boolean): ArrayList[T] = new ArrayList[T](underlying.filter(p))
}

private class IteratorImpl[T](underlying: scala.collection.Iterator[T]) extends Iterator[T] {
  def hasNext() = underlying.hasNext
  def next(): T = underlying.next()
}

trait Iterator[T] {
  def hasNext(): Boolean
  def next(): T
}

object Iterator {
  def apply[T](underlying: scala.collection.Iterator[T]): Iterator[T] = new IteratorImpl(underlying)
}

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

  def iterator(): Iterator[T] = Iterator(underlying.iterator)

  def toArray(a: Array[T]): Array[T] = {
    require(a.length == underlying.size)
    underlying.copyToArray(a)
    a
  }

  def isEmpty: Boolean = underlying.isEmpty
}

trait List[T] {
  def add(t: T): Unit
  def get(i: Int): T
  def set(i: Int, t: T): Unit
  def size: Int
  def iterator(): Iterator[T]
  def toArray[U >: T](a: Array[U]): Array[U]
  def isEmpty: Boolean
  def contains(t: T): Boolean
  def foreach[U](f: T ⇒ U): Unit
}

trait Map[K, V] {
  def get(k: K)(implicit ev: Null <:< V): V
  def put(k: K, v: V): Unit
  def remove(k: K): Unit
  def keysIterator(): Iterator[K]
  def containsKey(k: K): Boolean
  def containsValue(v: V): Boolean
  def foreach[U](f: ((K, V)) ⇒ U): Unit

  def withFilter(p: ((K, V)) ⇒ Boolean): sc.Iterator[(K, V)]
}

class LinkedList[T]() extends List[T] {

  private var underlying: i.List[T] = Nil

  override def toArray[U >: T](a: Array[U]): Array[U] = {
    require(a.length == underlying.size)
    underlying.copyToArray(a)
    a
  }

  def size: Int = underlying.size
  def addFirst(t: T): Unit = underlying ::= t

  // Unused by Saxon
  def add(t: T): Unit = ???
  def set(i: Int, t: T): Unit = ???
  def get(i: Int): T = ???
  def iterator(): Iterator[T] = ???
  def isEmpty: Boolean = ???
  def contains(t: T): Boolean = ???
  def foreach[U](f: (T) ⇒ U): Unit = ???
}

object Collections {

  def emptyIterator[T]: Iterator[T] = Iterator(sc.Iterator.empty)

  def emptyList[T](): List[T] = new List[T] {
    def add(t: T): Unit = throw new UnsupportedOperationException
    def set(i: Int, t: T): Unit = throw new UnsupportedOperationException
    def get(i: Int): T = throw new IndexOutOfBoundsException
    def toArray[U >: T](a: Array[U]): Array[U] = { require(a.isEmpty); a }
    def size: Int = 0
    def iterator(): Iterator[T] = emptyIterator
    def isEmpty: Boolean = true
    def contains(t: T): Boolean = false
    def foreach[U](f: (T) ⇒ U): Unit = ()
  }
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

  def signum(i: Long): Int = //ORBEON will be in Scala.js after 0.6.2
    if (i < 0L) -1 else if (i == 0L) 0 else 1

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