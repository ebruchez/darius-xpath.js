/*
*  Ported by Alistair Johnson from https://github.com/gwtproject/gwt/blob/master/user/src/com/google/gwt/core/shared/impl/InternalPreconditions.java
*/

package java.math

import java.util.NoSuchElementException

object InternalPreconditions {

  def checkType(expression: Boolean) {
    if (!expression) {
      throw new ClassCastException()
    }
  }

  def checkArrayType(expression: Boolean) {
    if (!expression) {
      throw new ArrayStoreException()
    }
  }

  def checkArrayType(expression: Boolean, errorMessage: AnyRef) {
    if (!expression) {
      throw new ArrayStoreException(String.valueOf(errorMessage))
    }
  }

  def checkElement(expression: Boolean) {
    checkCriticalElement(expression)
  }

  def checkElement(expression: Boolean, errorMessage: AnyRef) {
    if (!expression) {
      throw new NoSuchElementException(String.valueOf(errorMessage))
    }
  }

  def checkCriticalElement(expression: Boolean) {
    if (!expression) {
      throw new NoSuchElementException()
    }
  }

  def checkCriticalArgument(expression: Boolean) {
    if (!expression) {
      throw new IllegalArgumentException()
    }
  }

  def checkCriticalArgument(expression: Boolean, errorMessage: AnyRef) {
    if (!expression) {
      throw new IllegalArgumentException(String.valueOf(errorMessage))
    }
  }

  def checkState(expression: Boolean) {
    if (!expression) {
      throw new IllegalStateException()
    }
  }

  def checkState(expression: Boolean, errorMessage: AnyRef) {
    if (!expression) {
      throw new IllegalStateException(String.valueOf(errorMessage))
    }
  }

  def checkNotNull[T](reference: T): T = {
    if (reference == null) {
      throw new NullPointerException()
    }
    reference
  }

  def checkNotNull(reference: AnyRef, errorMessage: AnyRef) {
    if (reference == null) {
      throw new NullPointerException(String.valueOf(errorMessage))
    }
  }

  def checkArraySize(size: Int) {
    if (size < 0) {
      throw new NegativeArraySizeException("Negative array size: " + size)
    }
  }

  def checkElementIndex(index: Int, size: Int) {
    if (index < 0 || index >= size) {
      throw new IndexOutOfBoundsException("Index: " + index + ", Size: " + size)
    }
  }

  def checkPositionIndex(index: Int, size: Int) {
    if (index < 0 || index > size) {
      throw new IndexOutOfBoundsException("Index: " + index + ", Size: " + size)
    }
  }

  def checkPositionIndexes(start: Int, end: Int, size: Int) {
    checkCriticalPositionIndexes(start, end, size)
  }

  def checkCriticalPositionIndexes(start: Int, end: Int, size: Int) {
    if (start < 0) {
      throw new IndexOutOfBoundsException("fromIndex: " + start + " < 0")
    }
    if (end > size) {
      throw new IndexOutOfBoundsException("toIndex: " + end + " > size " + size)
    }
    if (start > end) {
      throw new IllegalArgumentException("fromIndex: " + start + " > toIndex: " + end)
    }
  }

  private def format(template: String, args: AnyRef*): String = {

    val _template = String.valueOf(template)
    val builder = new StringBuilder(_template.length + 16 * args.length)
    var templateStart = 0
    var i = 0
    while (i < args.length) {
      val placeholderStart = _template.indexOf("%s", templateStart)
      if (placeholderStart == -1) {
        //break
      }
      builder.append(_template.substring(templateStart, placeholderStart))
      i += 1
      builder.append(args(i))
      templateStart = placeholderStart + 2
    }
    builder.append(_template.substring(templateStart))
    if (i < args.length) {
      builder.append(" [")
      i += 1
      builder.append(args(i))
      while (i < args.length) {
        builder.append(", ")
        i += 1
        builder.append(args(i))
      }
      builder.append(']')
    }
    builder.toString
  }
}
