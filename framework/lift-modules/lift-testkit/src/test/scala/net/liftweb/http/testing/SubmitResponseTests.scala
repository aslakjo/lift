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
     "include the session" in {
       val response = new HttpResponse (
         "url", 200, "message", Map("Set-Cookie" -> List("value")),
         Full(xml.toString.toArray.map(_.toByte)),
         new HttpClient
       ) with PostListener {
         def headerVerifyer(headers : Array[Header]) = {
           headers.find(h =>  h.getName.equals("cookie") && h.getValue.equals("value")) match {
             case Some(_) => ;
             case None => error("Header not set.")
           }
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
    null
  }

  def headerVerifyer(header:Array[Header]):Unit
}

