package net.liftweb.http.testing


import _root_.org.specs._
import _root_.org.specs.matcher.Matcher
import net.liftweb.common.{Box, Full}
import _root_.org.apache.commons.httpclient._
import methods._ 




class SubmitResponseTests extends Specification{

  val xml = <html>
    <body>
      <form action="/action" >
        <input name="field" value="test"/>
      </form>
    </body>
  </html>


  "Submiting a reuqest" should {
     "include the session cookie in header" in {
       val cookieValue = "cookie-value"

       val response = new HttpResponse (
         "url", 200, "message", Map("Set-Cookie" -> List(cookieValue)),
         Full(xml.toString.toArray.map(_.toByte)),
         new HttpClient
       ) with PostListener {
         override def headerVerifyer(headers : Array[Header]) = {
           headers.find(h =>  h.getName.equals("cookie") && h.getValue.equals(cookieValue)) match {
             case Some(_) => ;
             case None => error("Header not set.")
           }
         }
       }
         response.submit(("test" -> "test"))
     }

    "include the pust values in the request" in {
      val response = new HttpResponse (
         "url", 200, "message", Map("Set-Cookie" -> List("cookie-value")),
         Full(xml.toString.toArray.map(_.toByte)),
         new HttpClient
       ) with PostListener {
         override def parameterVerifyer(httpMethod: PostMethod) ={
           if(!httpMethod.getParameter("test").getValue.equals("test"))
             error("Value is not set correctly")
         }
       }
         response.submit(("test" -> "test"))  
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

