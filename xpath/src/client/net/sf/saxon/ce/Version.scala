package client.net.sf.saxon.ce

//remove if not needed
import scala.collection.JavaConversions._

object Version {

  private val VERSION_PREFIX = ""

  private val MAJOR_VERSION = "1"

  private val MINOR_VERSION = "1"

  private val RELEASE_DATE = "2013-02-22"

  private val MAJOR_RELEASE_DATE = "2013-02-22"

  /**
   * Return the name of this product. Supports the XSLT 2.0 system property xsl:product-name
   *
   * @return the string "SAXON"
   */
  def getProductName(): String = "Saxon-" + Configuration.getEditionCode

  /**
   * Return the edition of this product - anticipated to remain 'CE'
   *
   * @return the string "CE" - used by Verifier
   */
  def getProductEdition(): String = Configuration.getEditionCode

  /**
   * Get the version number of the schema-aware version of the product
   *
   * @return the version number of this version of Saxon, as a string
   */
  def getProductVariantAndVersion(): String = {
    val edition = Configuration.getEditionCode
    edition + " " + getProductVersion
  }

  /**
   * Get the user-visible version number of this version of the product
   *
   * @return the version number of this version of Saxon, as a string: for example "9.0.1"
   */
  def getProductVersion(): String = {
    val prefixSeparator = if ((VERSION_PREFIX == "")) "" else " "
    VERSION_PREFIX + prefixSeparator + MAJOR_VERSION + "." + 
      MINOR_VERSION
  }

  /**
   * Get the suffix for the saxonce license file that matches this major version
   *
   * @return the suffix, as a string: for example "R2" for Release 2.3
   */
  def getLicenseFileName(): String = {
    val prefix = if ((VERSION_PREFIX == "")) "" else Character toString VERSION_PREFIX.charAt(0)
    "saxonce-license-" + prefix + MAJOR_VERSION + ".txt"
  }

  /**
   * Get the version of the XSLT specification that this product supports
   *
   * @return the string 2.0
   */
  def getXSLVersionString(): String = "2.0"

  /**
   * Get a message used to identify this product when a transformation is run using the -t option
   *
   * @return A string containing both the product name and the product
   *         version
   */
  def getProductTitle(): String = {
    getProductName + ' ' + getProductVersion + " from Saxonica"
  }

  /**
   * Return a web site address containing information about the product. Supports the XSLT system property xsl:vendor-url
   *
   * @return the string "http://www.saxonica.com/"
   */
  def getWebSiteAddress(): String = "http://www.saxonica.com/ce"

  /**
   * Invoking client.net.sf.saxon.ce.Version from the command line outputs the build number
   *
   * @param args not used
   */
  def main(args: Array[String]) {
    System.err.println(getProductTitle)
  }
}
