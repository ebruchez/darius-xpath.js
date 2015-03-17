package client.net.sf.saxon.ce.expr

import client.net.sf.saxon.ce.functions.SystemFunction
import client.net.sf.saxon.ce.om.Item
import client.net.sf.saxon.ce.om.SequenceIterator
import client.net.sf.saxon.ce.pattern.EmptySequenceTest
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.`type`._
import client.net.sf.saxon.ce.value.Cardinality
import client.net.sf.saxon.ce.value.SequenceTool
import client.net.sf.saxon.ce.value.SequenceType
//remove if not needed
import scala.collection.JavaConversions._

object TypeChecker {

  /**
   * Check an expression against a required type, modifying it if necessary.
   *
   * <p>This method takes the supplied expression and checks to see whether it is
   * known statically to conform to the specified type. There are three possible
   * outcomes. If the static type of the expression is a subtype of the required
   * type, the method returns the expression unchanged. If the static type of
   * the expression is incompatible with the required type (for example, if the
   * supplied type is integer and the required type is string) the method throws
   * an exception (this results in a compile-time type error being reported). If
   * the static type is a supertype of the required type, then a new expression
   * is constructed that evaluates the original expression and checks the dynamic
   * type of the result; this new expression is returned as the result of the
   * method.</p>
   *
   * <p>The rules applied are those for function calling in XPath, that is, the rules
   * that the argument of a function call must obey in relation to the signature of
   * the function. Some contexts require slightly different rules (for example,
   * operands of polymorphic operators such as "+"). In such cases this method cannot
   * be used.</p>
   *
   * <p>Note that this method does <b>not</b> do recursive type-checking of the
   * sub-expressions.</p>
   *
   *
   *
   * @param supplied      The expression to be type-checked
   * @param req           The required type for the context in which the expression is used
   * @param backwardsCompatible
   *                      True if XPath 1.0 backwards compatibility mode is applicable
   * @param role          Information about the role of the subexpression within the
   *                      containing expression, used to provide useful error messages
   * @return              The original expression if it is type-safe, or the expression
   *                      wrapped in a run-time type checking expression if not.
   * @throws XPathException if the supplied type is statically inconsistent with the
   *                      required type (that is, if they have no common subtype)
   */
  def staticTypeCheck(supplied: Expression, 
      req: SequenceType, 
      backwardsCompatible: Boolean, 
      role: RoleLocator): Expression = {
    if (supplied.implementsStaticTypeCheck()) {
      return supplied.staticTypeCheck(req, backwardsCompatible, role)
    }
    var exp = supplied
    val th = TypeHierarchy.getInstance
    val reqItemType = req.getPrimaryType
    val reqCard = req.getCardinality
    val allowsMany = Cardinality.allowsMany(reqCard)
    var suppliedItemType: ItemType = null
    var suppliedCard = -1
    var cardOK = (reqCard == StaticProperty.ALLOWS_ZERO_OR_MORE)
    if (!cardOK) {
      suppliedCard = exp.getCardinality
      cardOK = Cardinality.subsumes(reqCard, suppliedCard)
    }
    var itemTypeOK = reqItemType.isInstanceOf[AnyItemType]
    if (!itemTypeOK) {
      suppliedItemType = exp.getItemType
      if (suppliedItemType.isInstanceOf[EmptySequenceTest]) {
        itemTypeOK = true
      } else {
        if (reqItemType == null || suppliedItemType == null) {
          throw new NullPointerException()
        }
        val relation = th.relationship(reqItemType, suppliedItemType)
        itemTypeOK = relation == TypeHierarchy.SAME_TYPE || relation == TypeHierarchy.SUBSUMES
      }
    }
    if (backwardsCompatible && !allowsMany) {
      if (Cardinality.allowsMany(suppliedCard)) {
        val cexp = new FirstItemExpression(exp)
        cexp.adoptChildExpression(exp)
        exp = cexp
        suppliedCard = StaticProperty.ALLOWS_ZERO_OR_ONE
        cardOK = Cardinality.subsumes(reqCard, suppliedCard)
      }
      if (!itemTypeOK) {
        if (reqItemType == AtomicType.STRING) {
          exp = SystemFunction.makeSystemFunction("string", Array(exp))
          suppliedItemType = AtomicType.STRING
          suppliedCard = StaticProperty.EXACTLY_ONE
          cardOK = Cardinality.subsumes(reqCard, suppliedCard)
          itemTypeOK = true
        }
        if (reqItemType == AtomicType.NUMERIC || reqItemType == AtomicType.DOUBLE) {
          exp = SystemFunction.makeSystemFunction("number", Array(exp))
          suppliedItemType = AtomicType.DOUBLE
          suppliedCard = StaticProperty.EXACTLY_ONE
          cardOK = Cardinality.subsumes(reqCard, suppliedCard)
          itemTypeOK = true
        }
      }
    }
    if (!itemTypeOK) {
      if (reqItemType.isInstanceOf[AtomicType]) {
        if (!(suppliedItemType.isInstanceOf[AtomicType]) && !(suppliedCard == StaticProperty.EMPTY)) {
          exp = new Atomizer(exp)
          suppliedItemType = exp.getItemType
          suppliedCard = exp.getCardinality
          cardOK = Cardinality.subsumes(reqCard, suppliedCard)
        }
        if ((suppliedItemType == AtomicType.UNTYPED_ATOMIC) && 
          !(reqItemType == AtomicType.UNTYPED_ATOMIC || reqItemType == AtomicType.ANY_ATOMIC)) {
          exp = new UntypedAtomicConverter(exp, reqItemType.asInstanceOf[AtomicType], true, role)
          itemTypeOK = true
          suppliedItemType = reqItemType
        }
        if ((suppliedItemType == AtomicType.ANY_ATOMIC) && 
          !(reqItemType == AtomicType.UNTYPED_ATOMIC || reqItemType == AtomicType.ANY_ATOMIC) && 
          (exp.getSpecialProperties & StaticProperty.NOT_UNTYPED) == 
          0) {
          exp = new UntypedAtomicConverter(exp, reqItemType.asInstanceOf[AtomicType], false, role)
        }
        if ((reqItemType == AtomicType.DOUBLE && 
          th.relationship(suppliedItemType, AtomicType.NUMERIC) != 
          TypeHierarchy.DISJOINT)) {
          exp = new PromoteToDouble(exp)
          suppliedItemType = AtomicType.DOUBLE
          suppliedCard = -1
        } else if (reqItemType == AtomicType.FLOAT && 
          th.relationship(suppliedItemType, AtomicType.NUMERIC) != 
          TypeHierarchy.DISJOINT && 
          !th.isSubType(suppliedItemType, AtomicType.DOUBLE)) {
          exp = new PromoteToFloat(exp)
          suppliedItemType = (if (reqItemType == AtomicType.DOUBLE) AtomicType.DOUBLE else AtomicType.FLOAT)
          suppliedCard = -1
        }
        if (reqItemType == AtomicType.STRING && th.isSubType(suppliedItemType, AtomicType.ANY_URI)) {
          suppliedItemType = AtomicType.STRING
          itemTypeOK = true
        }
      }
    }
    if (itemTypeOK && cardOK) {
      return exp
    }
    if (suppliedCard == -1) {
      suppliedCard = exp.getCardinality
      if (!cardOK) {
        cardOK = Cardinality.subsumes(reqCard, suppliedCard)
      }
    }
    if (cardOK && suppliedCard == StaticProperty.EMPTY) {
      return exp
    }
    if (suppliedCard == StaticProperty.EMPTY && ((reqCard & StaticProperty.ALLOWS_ZERO) == 0)) {
      val err = new XPathException("An empty sequence is not allowed as the " + role.getMessage, supplied.getSourceLocator)
      err.setErrorCode(role.getErrorCode)
      err.setIsTypeError(true)
      throw err
    }
    val relation = (if (itemTypeOK) TypeHierarchy.SUBSUMED_BY else th.relationship(suppliedItemType, 
      reqItemType))
    if (relation == TypeHierarchy.DISJOINT) {
      if (Cardinality.allowsZero(suppliedCard) && Cardinality.allowsZero(reqCard)) {
      } else {
        val err = new XPathException("Required item type of " + role.getMessage + " is " + 
          reqItemType.toString + 
          "; supplied value has item type " + 
          suppliedItemType.toString, supplied.getSourceLocator)
        err.setErrorCode(role.getErrorCode)
        err.setIsTypeError(true)
        throw err
      }
    }
    if (!(relation == TypeHierarchy.SAME_TYPE || relation == TypeHierarchy.SUBSUMED_BY)) {
      if (exp.isInstanceOf[Literal]) {
        val err = new XPathException("Required item type of " + role.getMessage + " is " + 
          reqItemType.toString + 
          "; supplied value has item type " + 
          suppliedItemType.toString, supplied.getSourceLocator)
        err.setErrorCode(role.getErrorCode)
        err.setIsTypeError(true)
        throw err
      }
      val cexp = new ItemChecker(exp, reqItemType, role)
      ExpressionTool.copyLocationInfo(exp, cexp)
      exp = cexp
    }
    if (!cardOK) {
      if (exp.isInstanceOf[Literal]) {
        val err = new XPathException("Required cardinality of " + role.getMessage + " is " + 
          Cardinality toString reqCard + 
          "; supplied value has cardinality " + 
          Cardinality toString suppliedCard, supplied.getSourceLocator)
        err.setIsTypeError(true)
        err.setErrorCode(role.getErrorCode)
        throw err
      } else {
        val cexp = CardinalityChecker.makeCardinalityChecker(exp, reqCard, role)
        ExpressionTool.copyLocationInfo(exp, cexp)
        exp = cexp
      }
    }
    exp
  }

  /**
   * Test whether a given value conforms to a given type
   *
   * @param iter iterator over the value
   * @param requiredType the required type
   * @return a string describing the error condition if the value doesn't conform;
   * or null if it does.
   * @throws XPathException if a failure occurs reading the value
   */
  def testConformance(iter: SequenceIterator, requiredType: SequenceType): String = {
    val reqItemType = requiredType.getPrimaryType
    var count = 0
    while (true) {
      val item = iter.next()
      if (item == null) {
        //break
      }
      count += 1
      if (!reqItemType.matchesItem(item)) {
        return ("Required type is " + reqItemType + "; supplied value includes an item of type " + 
          SequenceTool.getItemType(item))
      }
    }
    val reqCardinality = requiredType.getCardinality
    if (count == 0 && !Cardinality.allowsZero(reqCardinality)) {
      return "Required type does not allow empty sequence, but supplied value is empty"
    }
    if (count > 1 && !Cardinality.allowsMany(reqCardinality)) {
      return "Required type requires a singleton sequence; supplied value contains " + 
        count + 
        " items"
    }
    if (count > 0 && reqCardinality == StaticProperty.EMPTY) {
      return "Required type requires an empty sequence, but supplied value is non-empty"
    }
    null
  }

  /**
   * Test whether a given expression is capable of returning a value that has an effective boolean
   * value.
   *
   * @param exp the given expression
   * @return null if the expression is OK (optimistically), an exception object if not
   */
  def ebvError(exp: Expression): XPathException = {
    if (Cardinality.allowsZero(exp.getCardinality)) {
      return null
    }
    val t = exp.getItemType
    val th = TypeHierarchy.getInstance
    if (th.relationship(t, Type.NODE_TYPE) == TypeHierarchy.DISJOINT && 
      th.relationship(t, AtomicType.BOOLEAN) == TypeHierarchy.DISJOINT && 
      th.relationship(t, AtomicType.STRING) == TypeHierarchy.DISJOINT && 
      th.relationship(t, AtomicType.ANY_URI) == TypeHierarchy.DISJOINT && 
      th.relationship(t, AtomicType.UNTYPED_ATOMIC) == TypeHierarchy.DISJOINT && 
      th.relationship(t, AtomicType.NUMERIC) == TypeHierarchy.DISJOINT) {
      val err = new XPathException("Effective boolean value is defined only for sequences containing " + 
        "booleans, strings, numbers, URIs, or nodes", "FORG0006")
      err.setIsTypeError(true)
      return err
    }
    null
  }
}
