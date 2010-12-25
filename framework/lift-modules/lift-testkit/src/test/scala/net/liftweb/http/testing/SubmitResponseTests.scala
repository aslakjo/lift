package net.liftweb.http.testing


import _root_.org.specs._
import _root_.org.specs.matcher.Matcher
import net.liftweb.common.{Box, Full}
import _root_.org.apache.commons.httpclient._
import methods._ 




class SubmitResponseTests extends Specification{
  var shouldBeenCalled =false
  val xml = <html>
    <body>
      <form action="/action" >
        <input name="field" value="test"/>

        <label for="fake-set-uuid">Label</label>
        <input id="fake-set-uuid" name="anotherField" value="value for label"/>

        <label for="nothing">Nothing</label>
      </form>
    </body>
  </html>


  "Submiting a reuqest" should {
    doBefore{ shouldBeenCalled = false}
     "include the session cookie in header" in {

       val cookieValue = "cookie-value"

       val response = new HttpResponse (
         "url", 200, "message", Map("Set-Cookie" -> List(cookieValue)),
         Full(xml.toString.toArray.map(_.toByte)),
         new HttpClient
       ) with PostListener {
         override def headerVerifyer(headers : Array[Header]) = {
           shouldBeenCalled = true
           headers.find(h =>  h.getName.equals("cookie") && h.getValue.equals(cookieValue)) match {
             case Some(_) => ;
             case None => error("Header not set.")
           }
         }
       }
        response.submit((Label("test") -> "test"))
        shouldBeenCalled must be(true)
     }

    "include the pust values in the request" in {
      val response = new HttpResponse (
         "url", 200, "message", Map("Set-Cookie" -> List("cookie-value")),
         Full(xml.toString.toArray.map(_.toByte)),
         new HttpClient
       ) with PostListener {
         override def parameterVerifyer(httpMethod: PostMethod) ={
           shouldBeenCalled = true
           if(!httpMethod.getParameter("test").getValue.equals("test"))
             error("Value is not set correctly")
         }
       }
         response.submit((Label("test") -> "test"))
         shouldBeenCalled must be(true)
    }

    "set values for the corrensponding labels" in {
       val response = new HttpResponse (
         "url", 200, "message", Map("Set-Cookie" -> List("cookie-value")),
         Full(xml.toString.toArray.map(_.toByte)),
         new HttpClient
       ) with PostListener {
         override def parameterVerifyer(httpMethod: PostMethod) ={
           shouldBeenCalled = true
           if(httpMethod.getParameter("anotherField").getValue.equals("given value"))
             Unit
           else
             error("Value is not set correctly")
         }
       }
        response.submit((Label("Label") -> "given value"))
        shouldBeenCalled must be(true)
    }
  }
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




