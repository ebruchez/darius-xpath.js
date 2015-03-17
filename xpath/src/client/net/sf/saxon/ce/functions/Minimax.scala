package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr._
import client.net.sf.saxon.ce.expr.sort.AtomicComparer
import client.net.sf.saxon.ce.expr.sort.DescendingComparer
import client.net.sf.saxon.ce.expr.sort.GenericAtomicComparer
import client.net.sf.saxon.ce.lib.StringCollator
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.trans.Err
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`._
import client.net.sf.saxon.ce.value._
import Minimax._
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

object Minimax {

  val MIN = 2

  val MAX = 3

  /**
   * Static method to evaluate the minimum or maximum of a sequence
   * @param iter Iterator over the input sequence
   * @param operation either {@link #MIN} or {@link #MAX}
   * @param atomicComparer an AtomicComparer used to compare values
   * @param ignoreNaN true if NaN values are to be ignored
   * @param context dynamic evaluation context
   * @return the min or max value in the sequence, according to the rules of the fn:min() or fn:max() functions
   * @throws XPathException typically if non-comparable values are found in the sequence
   */
  def minimax(iter: SequenceIterator, 
      operation: Int, 
      atomicComparer: AtomicComparer, 
      ignoreNaN: Boolean, 
      context: XPathContext): AtomicValue = {
    val th = TypeHierarchy.getInstance
    var foundDouble = false
    var foundFloat = false
    var foundNaN = false
    if (operation == MAX) {
      atomicComparer = new DescendingComparer(atomicComparer)
    }
    var min: AtomicValue = null
    var prim: AtomicValue = null
    while (true) {
      min = iter.next().asInstanceOf[AtomicValue]
      if (min == null) {
        return null
      }
      prim = min
      if (min.isInstanceOf[UntypedAtomicValue]) {
        min = new DoubleValue(StringToDouble.stringToNumber(min.getStringValue))
        prim = min
        foundDouble = true
      } else {
        if (prim.isInstanceOf[DoubleValue]) {
          foundDouble = true
        } else if (prim.isInstanceOf[FloatValue]) {
          foundFloat = true
        }
      }
      if (prim.isNaN) {
        if (ignoreNaN) {
        } else if (prim.isInstanceOf[DoubleValue]) {
          return min
        } else {
          foundNaN = true
          min = FloatValue.NaN
          //break
        }
      } else {
        if (!prim.getItemType.isOrdered) {
          val de = new XPathException("Type " + prim.getItemType + " is not an ordered type", "FORG0006")
          de.setIsTypeError(true)
          throw de
        }
        //break
      }
    }
    var lowestCommonSuperType = min.getItemType
    while (true) {
      val test = iter.next().asInstanceOf[AtomicValue]
      if (test == null) {
        //break
      }
      var test2 = test
      prim = test2
      if (test.isInstanceOf[UntypedAtomicValue]) {
        test2 = new DoubleValue(StringToDouble.stringToNumber(test.getStringValue))
        if (foundNaN) {
          return DoubleValue.NaN
        }
        prim = test2
        foundDouble = true
      } else {
        if (prim.isInstanceOf[DoubleValue]) {
          if (foundNaN) {
            return DoubleValue.NaN
          }
          foundDouble = true
        } else if (prim.isInstanceOf[FloatValue]) {
          foundFloat = true
        }
      }
      lowestCommonSuperType = Type.getCommonSuperType(lowestCommonSuperType, prim.getItemType).asInstanceOf[AtomicType]
      if (prim.isNaN) {
        if (ignoreNaN) {
        } else if (foundDouble) {
          return DoubleValue.NaN
        } else {
          foundNaN = true
        }
      } else {
        try {
          if (atomicComparer.compareAtomicValues(prim, min) < 0) {
            min = test2
          }
        } catch {
          case err: ClassCastException => {
            val de = new XPathException("Cannot compare " + min.getItemType + " with " + test2.getItemType, 
              "FORG0006")
            de.setIsTypeError(true)
            throw de
          }
        }
      }
    }
    if (foundNaN) {
      return FloatValue.NaN
    }
    if (foundDouble) {
      if (!(min.isInstanceOf[DoubleValue])) {
        min = min.convert(AtomicType.DOUBLE).asAtomic()
      }
    } else if (foundFloat) {
      if (!(min.isInstanceOf[FloatValue])) {
        min = min.convert(AtomicType.FLOAT).asAtomic()
      }
    }
    min.convert(lowestCommonSuperType).asAtomic()
  }
}

/**
 * This class implements the min() and max() functions
 */
class Minimax(operation: Int) extends CollatingFunction {

  this.operation = operation

  def newInstance(): Minimax = new Minimax(operation)

  private var argumentType: AtomicType = AtomicType.ANY_ATOMIC

  @BooleanBeanProperty
  var ignoreNaN: Boolean = false

  /**
   * Static analysis: prevent sorting of the argument
   */
  def checkArguments(visitor: ExpressionVisitor) {
    super.checkArguments(visitor)
    argument(0) = ExpressionTool.unsorted(visitor.getConfiguration, argument(0), false)
  }

  /**
   * Determine the cardinality of the function.
   */
  def computeCardinality(): Int = {
    var c = super.computeCardinality()
    if (!Cardinality.allowsZero(argument(0).getCardinality)) {
      c = StaticProperty.EXACTLY_ONE
    }
    c
  }

  /**
   * Perform optimisation of an expression and its subexpressions.
   * <p/>
   * <p>This method is called after all references to functions and variables have been resolved
   * to the declaration of the function or variable, and after all type checking has been done.</p>
   *
   * @param visitor an expression visitor
   * @param contextItemType the static type of "." at the point where this expression is invoked.
   *                        The parameter is set to null if it is known statically that the context item will be undefined.
   *                        If the type of the context item is not known statically, the argument is set to
   *                        {@link client.net.sf.saxon.ce.type.Type#ITEM_TYPE}
   * @return the original expression, rewritten if appropriate to optimize execution
   * @throws XPathException if an error is discovered during this phase
   *                                        (typically a type error)
   */
  def optimize(visitor: ExpressionVisitor, contextItemType: ItemType): Expression = {
    val th = TypeHierarchy.getInstance
    argumentType = argument(0).getItemType.getAtomizedItemType.asInstanceOf[AtomicType]
    val e = super.optimize(visitor, contextItemType)
    if (e != this) {
      return e
    }
    if (getNumberOfArguments == 1) {
      val card = argument(0).getCardinality
      if (!Cardinality.allowsMany(card) && 
        th.isSubType(argument(0).getItemType, AtomicType.NUMERIC)) {
        return argument(0)
      }
    }
    this
  }

  /**
   * Determine the item type of the value returned by the function
   *
   * @return the statically inferred type of the expression
   */
  def getItemType(): ItemType = {
    val t = Atomizer.getAtomizedItemType(argument(0), false)
    if (t == AtomicType.UNTYPED_ATOMIC) {
      AtomicType.DOUBLE
    } else {
      t
    }
  }

  /**
   * Evaluate the function
   */
  def evaluateItem(context: XPathContext): Item = {
    val comparer = getAtomicComparer(context)
    val iter = argument(0).iterate(context)
    try {
      minimax(iter, operation, comparer, ignoreNaN, context)
    } catch {
      case err: XPathException => {
        err.maybeSetLocation(getSourceLocator)
        throw err
      }
    }
  }

  def getAtomicComparer(context: XPathContext): AtomicComparer = {
    val collator = getCollator(1, context)
    var `type` = argumentType
    if (`type` == AtomicType.UNTYPED_ATOMIC) {
      `type` = AtomicType.DOUBLE
    }
    val comparer = GenericAtomicComparer.makeAtomicComparer(`type`, `type`, collator, context.getImplicitTimezone)
    comparer
  }
}
