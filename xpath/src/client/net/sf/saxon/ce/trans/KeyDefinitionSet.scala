package client.net.sf.saxon.ce.trans

import client.net.sf.saxon.ce.om.StructuredQName
import java.util.ArrayList
import java.util.List
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A set of xsl:key definitions in a stylesheet that share the same name
 */
class KeyDefinitionSet(var keyName: StructuredQName, var keySetNumber: Int) {

  var keyDefinitions: List[KeyDefinition] = new ArrayList(3)

  var collationName: String = _

  var backwardsCompatible: Boolean = _

  /**
   * Add a key definition to this set of key definitions. The caller is responsible for ensuring that
   * all key definitions in a key definition set have the same name
   * @param keyDef the key definition to be added
   * @throws XPathException if the key definition uses a different collation from others in the set
   */
  def addKeyDefinition(keyDef: KeyDefinition) {
    if (keyDefinitions.isEmpty) {
      collationName = keyDef.getCollationName
    } else {
      if ((collationName == null && keyDef.getCollationName != null) || 
        (collationName != null && collationName != keyDef.getCollationName)) {
        val err = new XPathException("All keys with the same name must use the same collation")
        err.setErrorCode("XTSE1220")
        throw err
      }
      val v = getKeyDefinitions
      for (i <- 0 until v.size) {
        val other = v.get(i).asInstanceOf[KeyDefinition]
        if (keyDef.getMatch == other.getMatch && keyDef.getBody == other.getBody) {
          return
        }
      }
    }
    if (keyDef.isBackwardsCompatible) {
      backwardsCompatible = true
    }
    keyDefinitions.add(keyDef)
  }

  /**
   * Get the name of the key definitions in this set (they all share the same name)
   * @return the name of these key definitions
   */
  def getKeyName(): StructuredQName = keyName

  /**
   * Get the KeySet number. This uniquely identifies the KeyDefinitionSet within a KeyManager
   * @return the unique number
   */
  def getKeySetNumber(): Int = keySetNumber

  /**
   * Get the key definitions in this set
   * @return the key definitions in this set
   */
  def getKeyDefinitions(): List[KeyDefinition] = keyDefinitions

  /**
   * Determine if the keys are to be evaluated in backwards compatible mode
   * @return true if backwards compatibility is in force for at least one of the keys in the set
   */
  def isBackwardsCompatible(): Boolean = backwardsCompatible
}
