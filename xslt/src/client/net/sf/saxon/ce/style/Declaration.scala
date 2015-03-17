package client.net.sf.saxon.ce.style

import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

/**
 * The object represents a declaration (that is, a top-level element) in a stylesheet.
 * A declaration exists within a stylesheet module and takes its import precedence
 * from that of the module. The declaration corresponds to a source element in a stylesheet
 * document. However, if a stylesheet module is imported twice with different precedences,
 * then two declarations may share the same source element.
 */
class Declaration(@BeanProperty var module: StylesheetModule, source: StyleElement)
    {

  @BeanProperty
  var sourceElement: StyleElement = source

  def getPrecedence(): Int = module.getPrecedence
}
