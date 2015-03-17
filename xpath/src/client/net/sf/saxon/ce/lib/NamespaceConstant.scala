package client.net.sf.saxon.ce.lib

//remove if not needed
import scala.collection.JavaConversions._

object NamespaceConstant {

  /**
   * A URI representing the null namespace (actually, an empty string)
   */
  val NULL = ""

  /**
   * The numeric URI code representing the null namespace (actually, zero)
   */
  val NULL_CODE = 0

  /**
   * The namespace code for the null namespace
   */
  val NULL_NAMESPACE_CODE = 0

  /**
   * Fixed namespace name for XML: "http://www.w3.org/XML/1998/namespace".
   */
  val XML = "http://www.w3.org/XML/1998/namespace"

  /**
   * Numeric code representing the XML namespace
   */
  val XML_CODE = 1

  /**
   * The namespace code for the XML namespace
   */
  val XML_NAMESPACE_CODE = 0x00010001

  /**
   * Fixed namespace name for XSLT: "http://www.w3.org/1999/XSL/Transform"
   */
  val XSLT = "http://www.w3.org/1999/XSL/Transform"

  /**
   * Numeric code representing the XSLT namespace
   */
  val XSLT_CODE = 2

  /**
   * Fixed namespace name for SAXON: "http://saxon.sf.net/"
   */
  val SAXON = "http://saxon.sf.net/"

  /**
   * Numeric code representing the SAXON namespace
   */
  val SAXON_CODE = 3

  /**
   * Namespace name for XML Schema: "http://www.w3.org/2001/XMLSchema"
   */
  val SCHEMA = "http://www.w3.org/2001/XMLSchema"

  /**
   * Numeric code representing the schema namespace
   */
  val SCHEMA_CODE = 4

  /**
   * XML-schema-defined namespace for use in instance documents ("xsi")
   */
  val SCHEMA_INSTANCE = "http://www.w3.org/2001/XMLSchema-instance"

  val XSI_CODE = 5

  /**
   * Standard namespace for Saxon "Interactive XSLT" extensions
   */
  val IXSL = "http://saxonica.com/ns/interactiveXSLT"

  /**
   * Standard namespace for global javascript methods (defined on the Window object)
   */
  val JS = "http://saxonica.com/ns/globalJS"

  /**
   * Namespace for pseudo-attributes of HTML DOM elements that represent
   * element properties: for example, the <code>checked</code> property
   * (which is not the same as the <code>checked</code> attribute) can be
   * accessed as prop:checked. Note that these attributes can be accessed
   * by name, but they are not included in the result of @* or @prop:*
   */
  val HTML_PROP = "http://saxonica.com/ns/html-property"

  /**
   * Namespace for pseudo-attributes of HTML DOM elements that represent
   * style properties: for example, the <code>checked</code> property
   * (which is not the same as the <code>checked</code> attribute) can be
   * accessed as prop:checked. Note that these attributes can be accessed
   * by name, but they are not included in the result of @* or @prop:*
   */
  val HTML_STYLE_PROP = "http://saxonica.com/ns/html-style-property"

  /**
   * The standard namespace for functions and operators
   */
  val FN = "http://www.w3.org/2005/xpath-functions"

  /**
   * The standard namespace for system error codes
   */
  val ERR = "http://www.w3.org/2005/xqt-errors"

  /**
   * Fixed namespace name for EXSLT/Common: "http://exslt.org/common"
   */
  val EXSLT_COMMON = "http://exslt.org/common"

  /**
   * Recognize the Microsoft namespace so we can give a suitably sarcastic error message
   */
  val MICROSOFT_XSL = "http://www.w3.org/TR/WD-xsl"

  /**
   * Namespace for XHTML
   */
  val XHTML = "http://www.w3.org/1999/xhtml"

  /**
   * Namespace for Scalable Vector Graphics
   */
  val SVG = "http://www.w3.org/2000/svg"

  /**
   * The XMLNS namespace (used in DOM)
   */
  val XMLNS = "http://www.w3.org/2000/xmlns/"

  /**
   * URI identifying the Unicode codepoint collation
   */
  val CODEPOINT_COLLATION_URI = "http://www.w3.org/2005/xpath-functions/collation/codepoint"

  /**
   * URI identifying case-insensitive collation
   */
  val CASE_INSENSITIVE_COLLATION_URI = "http://saxon.sf.net/collation/case-insensitive"

  /**
   * URI for the names of generated global variables
   */
  val SAXON_GENERATED_GLOBAL = SAXON + "generated-global-variable"

  /**
   * Determine whether a namespace is a reserved namespace
   */
  def isReserved(uri: String): Boolean = {
    if (uri == null) {
      return false
    }
    uri == XSLT || uri == FN || uri == XML || uri == SCHEMA || 
      uri == SCHEMA_INSTANCE
  }
}
