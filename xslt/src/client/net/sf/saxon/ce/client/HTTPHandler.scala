package client.net.sf.saxon.ce.client

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

  def setErrorMessage(value: String) {
    responseState = State.ERROR
    errorMessage = value
  }

  def doGet(url: String, callback: RequestCallback) {
    val builder = new RequestBuilder(RequestBuilder.GET, url)
    responseState = State.COMPLETED
    try {
      val response = builder.sendRequest(null, callback)
    } catch {
      case e: RequestException => {
        responseState = State.ERROR
        errorMessage = e.getMessage
      }
    }
  }
}
