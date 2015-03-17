package client.net.sf.saxon.ce

import client.net.sf.saxon.ce.dom.HTMLDocumentWrapper
import client.net.sf.saxon.ce.dom.HTMLDocumentWrapper.DocType
import client.net.sf.saxon.ce.dom.XMLDOM
import client.net.sf.saxon.ce.event.PipelineConfiguration
import client.net.sf.saxon.ce.expr.sort.CaseInsensitiveCollator
import client.net.sf.saxon.ce.expr.sort.CodepointCollator
import client.net.sf.saxon.ce.lib.ErrorListener
import client.net.sf.saxon.ce.lib.NamespaceConstant
import client.net.sf.saxon.ce.lib.StandardErrorListener
import client.net.sf.saxon.ce.lib.StringCollator
import client.net.sf.saxon.ce.om.DocumentInfo
import client.net.sf.saxon.ce.om.DocumentPool
import client.net.sf.saxon.ce.trans.XPathException
import client.net.sf.saxon.ce.tree.util.URI
import client.net.sf.saxon.ce.value.DateTimeValue
import com.google.gwt.dom.client.Document
import com.google.gwt.dom.client.Node
import com.google.gwt.user.client.Window
import java.util.Date
import java.util.logging.Logger
import Configuration._
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

object Configuration {

  /**
   * Get the edition code identifying this configuration: "CE" for "client edition"
   */
  def getEditionCode(): String = "CE"

  def getLocation(): URI = {
    var location: URI = null
    try {
      location = new URI(Window.Location.getHref)
    } catch {
      case err: Exception => 
    }
    location
  }

  private var ieVersion: Int = 0

  /**
   * Returns -1 if host is not IE or the version number when IE is found
   */
  def getIeVersion(): Int = {
    if (ieVersion == 0) {
      ieVersion = getNativeIEVersion
    }
    ieVersion
  }

  /* native */ def getNativeIEVersion(): Int
}

/**
 * This class holds details of user-selected configuration options for a set of transformations
 * and/or queries. When running XSLT, the preferred way of setting configuration options is via
 * the JAXP TransformerFactory interface, but the Configuration object provides a finer
 * level of control. As yet there is no standard API for XQuery, so the only way of setting
 * Configuration information is to use the methods on this class directly.
 * <p/>
 * <p>As well as holding configuration settings, this class acts as a factory for classes
 * providing service in particular areas: error handling, URI resolution, and the like. Some
 * of these services are chosen on the basis of the current platform (Java or .NET), some vary
 * depending whether the environment is schema-aware or not.</p>
 * <p/>
 * <p>The <code>Configuration</code> establishes the scope within which node identity is managed.
 * Every document belongs to a <code>Configuration</code>, and every node has a distinct identity
 * within that <code>Configuration</code>. In consequence, it is not possible for any query or
 * transformation to manipulate multiple documents unless they all belong to the same
 * <code>Configuration</code>.</p>
 * <p/>
 * <p>Since Saxon 8.4, the JavaDoc documentation for Saxon attempts to identify interfaces
 * that are considered stable, and will only be changed in a backwards-incompatible way
 * if there is an overriding reason to do so. These interfaces and methods are labelled
 * with the JavaDoc "since" tag. The value 8.n indicates a method in this category that
 * was introduced in Saxon version 8.n: or in the case of 8.4, that was present in Saxon 8.4
 * and possibly in earlier releases. (In some cases, these methods have been unchanged for
 * a long time.) Methods without a "since" tag, although public, are provided for internal
 * use or for use by advanced users, and are subject to change from one release to the next.
 * The presence of a "since" tag on a class or interface indicates that there are one or more
 * methods in the class that are considered stable; it does not mean that all methods are
 * stable.
 *
 * @since 8.4
 */
class Configuration {

  @BeanProperty
  var errorListener: ErrorListener = new StandardErrorListener()

  @BeanProperty
  var globalDocumentPool: DocumentPool = new DocumentPool()

  @BeanProperty
  var implicitTimezone: Int = DateTimeValue.fromJavaDate(new Date()).getTimezoneInMinutes

  private var sourceDocumentPool: DocumentPool = new DocumentPool()

  private var logger: Logger = Logger.getLogger("Configuration")

  private var nextDocumentNumber: Int = 0

  /**
   * Allocate a unique document number
   * @return a unique document number
   */
  def allocateDocumentNumber(): Int = {
    synchronized {
      nextDocumentNumber += 1
    }
  }

  def getHostPage(): DocumentInfo = {
    val page = Document.get
    new HTMLDocumentWrapper(page, page.getURL, this, DocType.UNKNOWN)
  }

  /**
   * Get the collation with a given collation name. If the collation name has
   * not been registered in this CollationMap, the CollationURIResolver registered
   * with the Configuration is called. If this cannot resolve the collation name,
   * it should return null.
   *
   * @param name the collation name (should be an absolute URI)
   * @return the StringCollator with this name if known, or null if not known
   */
  def getNamedCollation(name: String): StringCollator = {
    if (name == NamespaceConstant.CODEPOINT_COLLATION_URI) {
      CodepointCollator.getInstance
    } else if (name == NamespaceConstant.CASE_INSENSITIVE_COLLATION_URI) {
      CaseInsensitiveCollator.getInstance
    } else {
      null
    }
  }

  /**
   * Get the document pool. This is used only for source documents, not for stylesheet modules.
   * <p/>
   * This method is intended for internal use only.
   *
   * @return the source document pool
   */
  def getDocumentPool(): DocumentPool = sourceDocumentPool

  /**
   * Make a PipelineConfiguration from the properties of this Configuration
   *
   * @return a new PipelineConfiguration
   * @since 8.4
   */
  def makePipelineConfiguration(): PipelineConfiguration = {
    val pipe = new PipelineConfiguration()
    pipe.setConfiguration(this)
    pipe
  }

  /**
   * Issue a warning
   */
  def issueWarning(message: String) {
    logger.warning(message)
  }

  /**
   * Build a document, using specified options for parsing and building.
   *
   * @param url the URL of the document to be fetched and parsed.
   * @throws XPathException if the URL cannot be dereferenced or if parsing fails
   */
  def buildDocument(url: String): DocumentInfo = {
    if (url == "html:document") {
      return getHostPage
    }
    var xml: String = null
    xml = XMLDOM.makeHTTPRequest(url)
    var jsDoc: Document = null
    jsDoc = XMLDOM.parseXML(xml).asInstanceOf[Document]
    if (jsDoc.getDocumentElement == null) {
      throw new XPathException("null returned for " + url)
    }
    new HTMLDocumentWrapper(jsDoc, url, Configuration.this, DocType.NONHTML)
  }

  def wrapHTMLDocument(doc: com.google.gwt.dom.client.Document, uri: String): DocumentInfo = {
    new HTMLDocumentWrapper(doc, uri, Configuration.this, DocType.UNKNOWN)
  }

  def wrapXMLDocument(doc: Node, uri: String): DocumentInfo = {
    new HTMLDocumentWrapper(doc, uri, Configuration.this, DocType.NONHTML)
  }
}
