// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.darius.xpath.om


object CopyOptions {

  val LOCAL_NAMESPACES = 1

  val ALL_NAMESPACES = 2

  val SOME_NAMESPACES = LOCAL_NAMESPACES | ALL_NAMESPACES

  val TYPE_ANNOTATIONS = 4

  def includes(options: Int, option: Int): Boolean = (options & option) == option
}
