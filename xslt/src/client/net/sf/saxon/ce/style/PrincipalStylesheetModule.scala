// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.style

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.instruct.Executable
import client.net.sf.saxon.ce.functions.ConstructorFunctionLibrary
import client.net.sf.saxon.ce.functions.FunctionLibraryList
import client.net.sf.saxon.ce.functions.StandardFunction
import client.net.sf.saxon.ce.functions.SystemFunctionLibrary
import client.net.sf.saxon.ce.js.IXSLFunctionLibrary
import client.net.sf.saxon.ce.om.DocumentURI
import client.net.sf.saxon.ce.om.NamespaceBinding
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.trans.RuleManager
import client.net.sf.saxon.ce.trans.StripSpaceRules
import client.net.sf.saxon.ce.trans.XPathException
import java.util._
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Represents the stylesheet module at the root of the import tree, that is, the module
 * that includes or imports all the others. Note that this object is present at compile time only,
 * unlike the Executable, which also exists at run-time.
 */
class PrincipalStylesheetModule(sourceElement: XSLStylesheet, precedence: Int)
    extends StylesheetModule(sourceElement, precedence) {

  private var preparedStylesheet: Executable = _

  @BeanProperty
  var functionLibrary: FunctionLibraryList = _

  @BeanProperty
  var version: String = _

  private var globalVariableIndex: HashMap[StructuredQName, Declaration] = new HashMap[StructuredQName, Declaration](20)

  private var templateIndex: HashMap[StructuredQName, Declaration] = new HashMap[StructuredQName, Declaration](20)

  private var functionIndex: HashMap[Integer, HashMap[StructuredQName, Declaration]] = new HashMap(8)

  private var localParameterNumbers: HashMap[StructuredQName, Integer] = null

  private var numberOfAliases: Int = 0

  private var namespaceAliasList: List[Declaration] = new ArrayList[Declaration](5)

  private var namespaceAliasMap: HashMap[String, NamespaceBinding] = _

  private var aliasResultUriSet: Set[String] = _

  private var largestPatternStackFrame: Int = 0

  private var moduleCache: HashMap[DocumentURI, XSLStylesheet] = new HashMap[DocumentURI, XSLStylesheet](4)

  def setExecutable(preparedStylesheet: Executable) {
    this.preparedStylesheet = preparedStylesheet
  }

  def getExecutable(): Executable = preparedStylesheet

  def getPrincipalStylesheetModule(): PrincipalStylesheetModule = this

  /**
   * Create the function library
   */
  def createFunctionLibrary(): FunctionLibraryList = {
    functionLibrary = new FunctionLibraryList()
    val functionSet = StandardFunction.CORE | StandardFunction.XSLT
    functionLibrary.addFunctionLibrary(SystemFunctionLibrary.getSystemFunctionLibrary(functionSet))
    functionLibrary.addFunctionLibrary(new StylesheetFunctionLibrary(this, true))
    functionLibrary.addFunctionLibrary(ConstructorFunctionLibrary.getInstance)
    functionLibrary.addFunctionLibrary(new IXSLFunctionLibrary())
    functionLibrary.addFunctionLibrary(new StylesheetFunctionLibrary(this, false))
    functionLibrary
  }

  /**
   * Add a module to the cache
   * @param key the key to be used (based on the absolute URI)
   * @param module the stylesheet document tree corresponding to this absolute URI
   */
  def putStylesheetDocument(key: DocumentURI, module: XSLStylesheet) {
    moduleCache.put(key, module)
  }

  /**
   * Get a module from the cache
   * @param key the key to be used (based on the absolute URI)
   * @return the stylesheet document tree corresponding to this absolute URI
   */
  def getStylesheetDocument(key: DocumentURI): XSLStylesheet = moduleCache.get(key)

  /**
   * Preprocess does all the processing possible before the source document is available.
   * It is done once per stylesheet, so the stylesheet can be reused for multiple source
   * documents. The method is called only on the XSLStylesheet element representing the
   * principal stylesheet module
   */
  def preprocess() {
    spliceIncludes()
    buildIndexes()
    processAllAttributes()
    collectNamespaceAliases()
    for (decl <- topLevel) {
      val inst = decl.getSourceElement
      if (!inst.isActionCompleted(StyleElement.ACTION_FIXUP)) {
        inst.setActionCompleted(StyleElement.ACTION_FIXUP)
        inst.fixupReferences()
      }
    }
    val top = getSourceElement
    setInputTypeAnnotations(top.getInputTypeAnnotationsAttribute)
    var decl = new Declaration(this, top)
    if (!top.isActionCompleted(StyleElement.ACTION_VALIDATE)) {
      top.setActionCompleted(StyleElement.ACTION_VALIDATE)
      top.validate(decl)
      for (aTopLevel <- topLevel) {
        decl = aTopLevel
        decl.getSourceElement.validateSubtree(decl)
      }
    }
  }

  /**
   * Build indexes for selected top-level declarations
   */
  private def buildIndexes() {
    var i = topLevel.size - 1
    while (i >= 0) {
      val decl = topLevel.get(i)
      decl.getSourceElement.index(decl, this)
      i -= 1
    }
  }

  /**
   * Process the attributes of every node in the stylesheet
   */
  def processAllAttributes() {
    getSourceElement.processDefaultCollationAttribute("")
    getSourceElement.prepareAttributes()
    for (decl <- topLevel) {
      val inst = decl.getSourceElement
      if (!inst.isActionCompleted(StyleElement.ACTION_PROCESS_ATTRIBUTES)) {
        inst.setActionCompleted(StyleElement.ACTION_PROCESS_ATTRIBUTES)
        try {
          inst.processAllAttributes()
        } catch {
          case err: XPathException => decl.getSourceElement.compileError(err)
        }
      }
    }
  }

  /**
   * Add a stylesheet function to the index
   * @param decl The declaration wrapping an XSLFunction object
   * @throws XPathException
   */
  protected def indexFunction(decl: Declaration) {
    val function = decl.getSourceElement.asInstanceOf[XSLFunction]
    val qName = function.getObjectName
    val arity = function.getNumberOfArguments
    val other = getFunctionDeclaration(qName, arity)
    if (other == null) {
      putFunction(decl)
    } else {
      val thisPrecedence = decl.getPrecedence
      val otherPrecedence = other.getPrecedence
      if (thisPrecedence == otherPrecedence) {
        val f2 = other.getSourceElement
        if (decl.getSourceElement == f2) {
          function.compileError("Function " + qName.getDisplayName + " is declared more than once " + 
            "(caused by including the containing module more than once)", "XTSE0770")
        } else {
          function.compileError("Duplicate function declaration", "XTSE0770")
        }
      } else if (thisPrecedence < otherPrecedence) {
      } else {
        putFunction(decl)
      }
    }
  }

  protected def getFunctionDeclaration(name: StructuredQName, arity: Int): Declaration = {
    val m = functionIndex.get(arity)
    (if (m == null) null else m.get(name))
  }

  /**
   * Get the function with a given name and arity
   * @param name the name of the function
   * @param arity the arity of the function, or -1 if any arity will do
   * @return the requested function, or null if none can be found
   */
  protected def getFunction(name: StructuredQName, arity: Int): XSLFunction = {
    if (arity == -1) {
      var arities = functionIndex.keySet.iterator()
      while (arities.hasNext) {
        val a = arities.next()
        val decl = getFunctionDeclaration(name, a)
        if (decl != null) {
          return decl.getSourceElement.asInstanceOf[XSLFunction]
        }
      }
      null
    } else {
      val decl = getFunctionDeclaration(name, arity)
      (if (decl == null) null else decl.getSourceElement.asInstanceOf[XSLFunction])
    }
  }

  protected def putFunction(decl: Declaration) {
    val function = decl.getSourceElement.asInstanceOf[XSLFunction]
    val qName = function.getObjectName
    val arity = function.getNumberOfArguments
    var m = functionIndex.get(arity)
    if (m == null) {
      m = new HashMap[StructuredQName, Declaration]()
      functionIndex.put(arity, m)
    }
    m.put(qName, decl)
  }

  /**
   * Index a global xsl:variable or xsl:param element
   * @param decl The Declaration referencing the XSLVariable or XSLParam element
   * @throws XPathException
   */
  protected def indexVariableDeclaration(decl: Declaration) {
    val `var` = decl.getSourceElement.asInstanceOf[XSLVariableDeclaration]
    val qName = `var`.getVariableQName
    if (qName != null) {
      val other = globalVariableIndex.get(qName)
      if (other == null) {
        globalVariableIndex.put(qName, decl)
      } else {
        val thisPrecedence = decl.getPrecedence
        val otherPrecedence = other.getPrecedence
        if (thisPrecedence == otherPrecedence) {
          val v2 = other.getSourceElement
          if (v2 == `var`) {
            `var`.compileError("Global variable " + qName.getDisplayName + " is declared more than once " + 
              "(caused by including the containing module more than once)", "XTSE0630")
          } else {
            `var`.compileError("Duplicate global variable declaration", "XTSE0630")
          }
        } else if (thisPrecedence < otherPrecedence && `var` != other.getSourceElement) {
          `var`.setRedundant()
        } else if (`var` != other.getSourceElement) {
          other.getSourceElement.asInstanceOf[XSLVariableDeclaration]
            .setRedundant()
          globalVariableIndex.put(qName, decl)
        }
      }
    }
  }

  /**
   * Get the global variable or parameter with a given name (taking
   * precedence rules into account)
   * @param qName name of the global variable or parameter
   * @return the variable declaration, or null if it does not exist
   */
  def getGlobalVariable(qName: StructuredQName): XSLVariableDeclaration = {
    val decl = globalVariableIndex.get(qName)
    (if (decl == null) null else decl.getSourceElement.asInstanceOf[XSLVariableDeclaration])
  }

  /**
   * Allocate a unique number to a local parameter name. This should only be called on the principal
   * stylesheet module.
   * @param qName the local parameter name
   * @return an integer that uniquely identifies this parameter name within the stylesheet
   */
  def allocateUniqueParameterNumber(qName: StructuredQName): Int = {
    if (localParameterNumbers == null) {
      localParameterNumbers = new HashMap[StructuredQName, Integer](50)
    }
    var x = localParameterNumbers.get(qName)
    if (x == null) {
      x = localParameterNumbers.size
      localParameterNumbers.put(qName, x)
    }
    x
  }

  /**
   * Add a named template to the index
   * @param decl the declaration of the Template object
   * @throws XPathException
   */
  protected def indexNamedTemplate(decl: Declaration) {
    val template = decl.getSourceElement.asInstanceOf[XSLTemplate]
    val qName = template.getTemplateName
    if (qName != null) {
      val other = templateIndex.get(qName)
      if (other == null) {
        templateIndex.put(qName, decl)
        getExecutable.putNamedTemplate(qName, template.getCompiledTemplate)
      } else {
        val thisPrecedence = decl.getPrecedence
        val otherPrecedence = other.getPrecedence
        if (thisPrecedence == otherPrecedence) {
          val t2 = other.getSourceElement
          template.compileError("Duplicate named template", "XTSE0660")
        } else if (thisPrecedence < otherPrecedence) {
        } else {
          templateIndex.put(qName, decl)
          getExecutable.putNamedTemplate(qName, template.getCompiledTemplate)
        }
      }
    }
  }

  /**
   * Get the named template with a given name
   * @param name the name of the required template
   * @return the template with the given name, if there is one, or null otherwise. If there
   * are several templates with the same name, the one with highest import precedence
   * is returned.
   */
  def getNamedTemplate(name: StructuredQName): XSLTemplate = {
    val decl = templateIndex.get(name)
    (if (decl == null) null else decl.getSourceElement.asInstanceOf[XSLTemplate])
  }

  protected def addNamespaceAlias(node: Declaration) {
    namespaceAliasList.add(node)
    numberOfAliases += 1
  }

  /**
   * Get the declared namespace alias for a given namespace URI code if there is one.
   * If there is more than one, we get the last.
   * @param uri The code of the uri used in the stylesheet.
   * @return The namespace code to be used (prefix in top half, uri in bottom half): return -1
   * if no alias is defined
   */
  protected def getNamespaceAlias(uri: String): NamespaceBinding = namespaceAliasMap.get(uri)

  /**
   * Determine if a namespace is included in the result-prefix of a namespace-alias
   * @param uri the  URI
   * @return true if an xsl:namespace-alias has been defined for this namespace URI
   */
  protected def isAliasResultNamespace(uri: String): Boolean = aliasResultUriSet.contains(uri)

  /**
   * Collect any namespace aliases
   */
  private def collectNamespaceAliases() {
    namespaceAliasMap = new HashMap(numberOfAliases)
    aliasResultUriSet = new HashSet(numberOfAliases)
    val aliasesAtThisPrecedence = new HashSet[String]()
    var currentPrecedence = -1
    for (i <- 0 until numberOfAliases) {
      val decl = namespaceAliasList.get(i)
      val xna = decl.getSourceElement.asInstanceOf[XSLNamespaceAlias]
      val scode = xna.getStylesheetURI
      val ncode = xna.getResultNamespaceBinding
      val prec = decl.getPrecedence
      if (currentPrecedence != prec) {
        currentPrecedence = prec
        aliasesAtThisPrecedence.clear()
      }
      if (aliasesAtThisPrecedence.contains(scode)) {
        if (namespaceAliasMap.get(scode) != ncode.getURI) {
          xna.compileError("More than one alias is defined for the same namespace", "XTSE0810")
        }
      }
      if (namespaceAliasMap.get(scode) == null) {
        namespaceAliasMap.put(scode, ncode)
        aliasResultUriSet.add(ncode.getURI)
      }
      aliasesAtThisPrecedence.add(scode)
    }
    namespaceAliasList = null
  }

  protected def hasNamespaceAliases(): Boolean = numberOfAliases > 0

  /**
   * Compile the stylesheet to create an executable.
   */
  def compileStylesheet() {
    try {
      val pss = getExecutable
      for (i <- 0 until topLevel.size) {
        val decl = topLevel.get(i)
        val snode = decl.getSourceElement
        if (snode.isInstanceOf[XSLTemplate]) {
          snode.asInstanceOf[XSLTemplate].register(decl)
        }
      }
      for (i <- 0 until topLevel.size) {
        val decl = topLevel.get(i)
        val snode = decl.getSourceElement
        if (!snode.isActionCompleted(StyleElement.ACTION_COMPILE)) {
          snode.setActionCompleted(StyleElement.ACTION_COMPILE)
          val inst = snode.compile(pss, decl)
          if (inst != null) {
            inst.setSourceLocator(snode)
          }
        }
      }
      var arities = functionIndex.keySet.iterator()
      while (arities.hasNext) {
        var fi = functionIndex.get(arities.next()).values.iterator()
        while (fi.hasNext) {
          val decl = fi.next()
          val node = decl.getSourceElement
          if (!node.isActionCompleted(StyleElement.ACTION_TYPECHECK)) {
            node.setActionCompleted(StyleElement.ACTION_TYPECHECK)
            node.asInstanceOf[XSLFunction].typeCheckBody()
          }
        }
      }
      if (getExecutable.getErrorCount > 0) {
        return
      }
      for (i <- 0 until topLevel.size) {
        val decl = topLevel.get(i)
        val node = decl.getSourceElement
        if (node.isInstanceOf[StylesheetProcedure] && !(node.isInstanceOf[XSLFunction]) && 
          !node.isActionCompleted(StyleElement.ACTION_OPTIMIZE)) {
          node.setActionCompleted(StyleElement.ACTION_OPTIMIZE)
          node.asInstanceOf[StylesheetProcedure].optimize(decl)
        }
      }
      var arities = functionIndex.keySet.iterator()
      while (arities.hasNext) {
        var fi = functionIndex.get(arities.next()).values.iterator()
        while (fi.hasNext) {
          val decl = fi.next()
          val node = decl.getSourceElement
          if (!node.isActionCompleted(StyleElement.ACTION_OPTIMIZE)) {
            node.setActionCompleted(StyleElement.ACTION_OPTIMIZE)
            node.asInstanceOf[StylesheetProcedure].optimize(decl)
          }
        }
      }
      if (pss.getDecimalFormatManager != null) {
        try {
          pss.getDecimalFormatManager.fixupDefaultDefault()
        } catch {
          case err: XPathException => compileError(err.getMessage, err.getErrorCodeLocalPart)
        }
      }
      val ruleManager = getExecutable.getRuleManager
      ruleManager.computeRankings()
    } catch {
      case err: RuntimeException => if (getExecutable.getErrorCount == 0) {
        throw err
      }
    }
  }

  /**
   * Get the list of attribute-set declarations associated with a given QName.
   * This is used for xsl:element, xsl:copy, xsl:attribute-set, and on literal
   * result elements
   *
   * @param name  the name of the required attribute set
   * @param list a list to hold the list of XSLAttributeSet elements in the stylesheet tree.
   * @return true if any declarations were found and added to the list; false if none were found
   */
  protected def getAttributeSets(name: StructuredQName, list: List[Declaration]): Boolean = {
    var found = false
    for (decl <- topLevel if decl.getSourceElement.isInstanceOf[XSLAttributeSet]) {
      val t = decl.getSourceElement.asInstanceOf[XSLAttributeSet]
      if (t.getAttributeSetName == name) {
        list.add(decl)
        found = true
      }
    }
    found
  }

  /**
   * Ensure there is enough space for local variables or parameters when evaluating the match pattern of
   * template rules
   * @param n the number of slots to be allocated
   */
  def allocatePatternSlots(n: Int) {
    if (n > largestPatternStackFrame) {
      largestPatternStackFrame = n
    }
  }

  /**
   * Compile time error, specifying an error code
   * @param message   the error message
   * @param errorCode the error code. May be null if not known or not defined
   * @throws XPathException
   */
  protected def compileError(message: String, errorCode: String) {
    val tce = new XPathException(message)
    tce.setErrorCode(errorCode)
    compileError(tce)
  }

  /**
   * Report an error with diagnostic information
   * @param error contains information about the error
   * @throws XPathException always, after reporting the error to the ErrorListener
   */
  protected def compileError(error: XPathException) {
    error.setIsStaticError(true)
    val pss = getExecutable
    if (pss == null) {
      throw error
    } else {
      pss.reportError(error)
    }
  }
}
