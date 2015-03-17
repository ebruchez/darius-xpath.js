package client.net.sf.saxon.ce.trans

import client.net.sf.saxon.ce.expr.Expression
import client.net.sf.saxon.ce.expr.XPathContext
import client.net.sf.saxon.ce.expr.instruct.Template
import client.net.sf.saxon.ce.om.NodeInfo
import client.net.sf.saxon.ce.om.StructuredQName
import client.net.sf.saxon.ce.pattern.Pattern
import client.net.sf.saxon.ce.pattern.UnionPattern
import client.net.sf.saxon.ce.style.StylesheetModule
import java.util.ArrayList
import java.util.HashMap
import java.util.Iterator
import java.util.List
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

/**
 * <B>RuleManager</B> maintains a set of template rules, one set for each mode
 * @version 10 December 1999: carved out of the old Controller class
 * @author Michael H. Kay
 */
class RuleManager {

  @BeanProperty
  var unnamedMode: Mode = _

  private var modes: HashMap[StructuredQName, Mode] = _

  private var omniMode: Mode = null

  private var recoveryPolicy: Int = _

  resetHandlers()

  /**
   * Set up a new table of handlers.
   */
  def resetHandlers() {
    unnamedMode = new Mode(Mode.UNNAMED_MODE, Mode.UNNAMED_MODE_NAME)
    modes = new HashMap[StructuredQName, Mode](5)
  }

  /**
   * Get the Mode object for a named mode. If there is not one already registered.
   * a new Mode is created.
   * @param modeName The name of the mode. Supply null to get the default
   * mode or Mode.ALL_MODES to get the Mode object containing "mode=all" rules
   * @param createIfAbsent if true, then if the mode does not already exist it will be created.
   * If false, then if the mode does not already exist the method returns null.
   * @return the Mode with this name
   */
  def getMode(modeName: StructuredQName, createIfAbsent: Boolean): Mode = {
    if (modeName == null || modeName == Mode.UNNAMED_MODE_NAME) {
      return unnamedMode
    }
    if (modeName == Mode.ALL_MODES) {
      if (omniMode == null) {
        omniMode = new Mode(Mode.NAMED_MODE, modeName)
      }
      return omniMode
    }
    var m = modes.get(modeName)
    if (m == null && createIfAbsent) {
      m = new Mode(omniMode, modeName)
      modes.put(modeName, m)
    }
    m
  }

  /**
   * Get all the modes in a given namespace
   * @param namespace the namespace URI
   * @return a list of modes whose names are in this namespace
   */
  def getModesInNamespace(namespace: String): List[Mode] = {
    val result = new ArrayList[Mode]()
    for (name <- modes.keySet if namespace == name.getNamespaceURI) {
      result.add(modes.get(name))
    }
    result
  }

  /**
   * Register a template for a particular pattern.
   * @param pattern Must be a valid Pattern.
   * @param eh The Template to be used
   * @param mode The processing mode to which this template applies
   * @param module The stylesheet module containing the template rule
   * @param priority The priority of the rule: if an element matches several patterns, the
   * one with highest priority is used
   * @see Pattern
   */
  def setTemplateRule(pattern: Pattern, 
      eh: Template, 
      mode: Mode, 
      module: StylesheetModule, 
      priority: Double, 
      ixslPreventDefault: Boolean, 
      ixslEventProperty: String) {
    if (pattern.isInstanceOf[UnionPattern]) {
      val up = pattern.asInstanceOf[UnionPattern]
      val p1 = up.getLHS
      val p2 = up.getRHS
      val currentSetter = up.getVariableBindingExpression
      if (currentSetter != null) {
        p1.setVariableBindingExpression(currentSetter)
        p2.setVariableBindingExpression(currentSetter)
      }
      setTemplateRule(p1, eh, mode, module, priority, ixslPreventDefault, ixslEventProperty)
      setTemplateRule(p2, eh, mode, module, priority, ixslPreventDefault, ixslEventProperty)
      return
    }
    if (Double.isNaN(priority)) {
      priority = pattern.getDefaultPriority
    }
    mode.addRule(pattern, eh, module, priority, true, ixslPreventDefault, ixslEventProperty)
    if (mode == omniMode) {
      unnamedMode.addRule(pattern, eh, module, priority, false, ixslPreventDefault, ixslEventProperty)
      val iter = modes.values.iterator()
      while (iter.hasNext) {
        val m = iter.next()
        m.addRule(pattern, eh, module, priority, false, ixslPreventDefault, ixslEventProperty)
      }
    }
  }

  /**
   * Find the template rule registered for a particular node in a specific mode.
   * @param node The NodeInfo for the relevant node
   * @param mode The processing mode
   * @param c The controller for this transformation
   * @return The template rule that will process this node
   * Returns null if there is no specific handler registered.
   */
  def getTemplateRule(node: NodeInfo, mode: Mode, c: XPathContext): Rule = {
    if (mode == null) {
      mode = unnamedMode
    }
    mode.getRule(node, c)
  }

  /**
   * Get a template rule whose import precedence is in a particular range. This is used to support
   * the xsl:apply-imports function
   * @param node The node to be matched
   * @param mode The mode for which a rule is required
   * @param min  The minimum import precedence that the rule must have
   * @param max  The maximum import precedence that the rule must have
   * @param c    The Controller for the transformation
   * @return     The template rule to be invoked
   * @throws XPathException
   */
  def getTemplateRule(node: NodeInfo, 
      mode: Mode, 
      min: Int, 
      max: Int, 
      c: XPathContext): Rule = {
    if (mode == null) {
      mode = unnamedMode
    }
    mode.getRule(node, min, max, c)
  }

  /**
   * Get the next-match handler after the current one
   * @param node  The node to be matched
   * @param mode  The processing mode
   * @param currentRule The current template rule
   * @param c     The dynamic context for the transformation
   * @return      The template rule to be executed
   * @throws XPathException
   */
  def getNextMatchHandler(node: NodeInfo, 
      mode: Mode, 
      currentRule: Rule, 
      c: XPathContext): Rule = {
    if (mode == null) {
      mode = unnamedMode
    }
    mode.getNextMatchRule(node, currentRule, c)
  }

  /**
   * Allocate rankings to the rules within each mode. This method must be called when all
   * the rules in each mode are known
   */
  def computeRankings() {
    unnamedMode.computeRankings()
    val iter = modes.values.iterator()
    while (iter.hasNext) {
      val mode = iter.next()
      mode.computeRankings()
    }
  }
}
