package net.liftweb.http.testing

import _root_.org.apache.commons.httpclient._
import methods._


case class NoLabelFound(val label:String) extends Exception("Label not found: " + label)
{

}

trait PostListener {
  self : HttpResponse=>
  type HttpResponse
  override def responseCapture(fullUrl: String,
                                       httpClient: HttpClient,
                                       getter: HttpMethodBase): ResponseType = {
    headerVerifyer(getter.getRequestHeaders)
    parameterVerifyer(
      if(getter.isInstanceOf[PostMethod])
        getter.asInstanceOf[PostMethod]
      else
        null
    )
    null
  }

  def headerVerifyer(header:Array[Header]):Unit = Unit
  def parameterVerifyer(httpMethod: PostMethod):Unit =Unit
}