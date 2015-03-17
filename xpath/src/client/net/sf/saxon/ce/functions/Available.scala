package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.ExpressionVisitor
import client.net.sf.saxon.ce.expr.StaticContext
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.style.StyleNodeFactory
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`.AtomicType
import client.net.sf.saxon.ce.value.BooleanValue
import client.net.sf.saxon.ce.value.NumericValue
import Available._
//remove if not needed
import scala.collection.JavaConversions._

object Available {

  val ELEMENT_AVAILABLE = 0

  val FUNCTION_AVAILABLE = 1

  val TYPE_AVAILABLE = 2
}

/**
 * This class supports the XSLT element-available and function-available functions.
 */
class Available(operation: Int) extends SystemFunction {

  this.operation = operation

  def newInstance(): Available = new Available(operation)

  private var env: StaticContext = _

  def checkArguments(visitor: ExpressionVisitor) {
    if (env == null) {
      env = visitor.getStaticContext
    }
  }

  /**
   * Get the effective boolean value of the expression. This returns false if the value
   * is the empty sequence, a zero-length string, a number equal to zero, or the boolean
   * false. Otherwise it returns true.
   *
   * @param context The context in which the expression is to be evaluated
   * @return the effective boolean value
   * @throws client.net.sf.saxon.ce.trans.XPathException
   *          if any dynamic error occurs evaluating the
   *          expression
   */
  override def effectiveBooleanValue(context: XPathContext): Boolean = {
    val lexicalQName = argument(0).evaluateAsString(context).toString
    operation match {
      case ELEMENT_AVAILABLE => {
        val qName = StructuredQName.fromLexicalQName(lexicalQName, env.getDefaultElementNamespace, env.getNamespaceResolver)
        new StyleNodeFactory(context.getConfiguration).isElementAvailable(qName.getNamespaceURI, qName.getLocalName)
      }
      case FUNCTION_AVAILABLE => {
        var arity = -1
        if (argument.length == 2) {
          arity = argument(1).evaluateItem(context).asInstanceOf[NumericValue]
            .intValue()
        }
        try {
          val qName = StructuredQName.fromLexicalQName(lexicalQName, env.getDefaultFunctionNamespace, 
            env.getNamespaceResolver)
          (env.getFunctionLibrary.hasFunctionSignature(qName, arity.toInt))
        } catch {
          case e2: XPathException => {
            e2.setErrorCode("XTDE1400")
            throw e2
          }
        }
      }
      case TYPE_AVAILABLE => {
        try {
          val qName = StructuredQName.fromLexicalQName(lexicalQName, env.getDefaultElementNamespace, 
            env.getNamespaceResolver)
          qName.getNamespaceURI == NamespaceConstant.SCHEMA && AtomicType.isRecognizedName(qName.getLocalName)
        } catch {
          case e: XPathException => {
            e.setErrorCode("XTDE1425")
            throw e
          }
        }
      }
      case _ => false
    }
  }

  /**
   * Run-time evaluation. This is the only thing in the spec that requires information
   * about in-scope functions to be available at run-time.
   */
  def evaluateItem(context: XPathContext): Item = {
    BooleanValue.get(effectiveBooleanValue(context))
  }
}
