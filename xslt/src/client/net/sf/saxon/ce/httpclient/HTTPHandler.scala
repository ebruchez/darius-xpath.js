// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is “Incompatible With Secondary Licenses”, as defined by the Mozilla Public License, v. 2.0.
package client.net.sf.saxon.ce.httpclient

import com.google.gwt.dom.client.Node
import com.google.gwt.http.client.Request
import com.google.gwt.http.client.RequestBuilder
import com.google.gwt.http.client.RequestCallback
import com.google.gwt.http.client.RequestException
import HTTPHandler._
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

object HTTPHandler {

  object State extends Enumeration {

    val NONE = new State()

    val COMPLETED = new State()

    val ERROR = new State()

    class State extends Val

    implicit def convertValue(v: Value): State = v.asInstanceOf[State]
  }
}

class HTTPHandler {

  var waitCount: Int = 1

  @BeanProperty
  var responseState: State = State.NONE

  @BeanProperty
  var errorMessage: String = ""

  @BeanProperty
  var resultNode: Node = null

  def setErrorMessage(value: String): Unit = {
    responseState = State.ERROR
    errorMessage = value
  }

  def doGet(url: String, callback: RequestCallback): Unit = {
    val builder = new RequestBuilder(RequestBuilder.GET, url)
    responseState = State.COMPLETED
    try {
      val response = builder.sendRequest(null, callback)
    } catch {
      case e: RequestException ⇒ {
        responseState = State.ERROR
        errorMessage = e.getMessage
      }
    }
  }
}
