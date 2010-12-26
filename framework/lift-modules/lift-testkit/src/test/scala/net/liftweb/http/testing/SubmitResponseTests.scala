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
        <label for="field">field</label>
        <input id="field" name="field" value="test"/>

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
        response.submit((Label("field") -> "test"))
        shouldBeenCalled must be(true)
     }

    "include the input values in the request" in {
      val response = new HttpResponse (
         "url", 200, "message", Map("Set-Cookie" -> List("cookie-value")),
         Full(xml.toString.toArray.map(_.toByte)),
         new HttpClient
       ) with PostListener {
         override def parameterVerifyer(httpMethod: PostMethod) ={
           shouldBeenCalled = true
           if(!httpMethod.getParameter("field").getValue.equals("new test value"))
             error("Value is not set correctly")
         }
       }
      
       response.submit((Label("field") -> "new test value"))
       shouldBeenCalled must be(true)
    }

    "set values for the corrensponding labels" in {
      val response = new HttpResponse (
       "url", 200, "message", Map("Set-Cookie" -> List("cookie-value")), Full(xml.toString.toArray.map(_.toByte)), new HttpClient
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

    "should throw exception when input field is not found" in {
      val response = new HttpResponse (
         "url", 200, "message", Map("Set-Cookie" -> List("cookie-value")), Full(xml.toString.toArray.map(_.toByte)), new HttpClient
       ) with PostListener

        try{
          response.submit((Label("Non_existent_label") -> "Not set value"))
          error("No exception thrown")
        }catch{
          case e:NoLabelFound => {
            e.getMessage must_== "Label not found: Non_existent_label"
          }
          case e@_ => error("Wrong exception thrown: " + e)
        }
        pass
    }

    "should set all input corresponding to labels to correct values" in {
      val response = new HttpResponse (
       "url", 200, "message", Map("Set-Cookie" -> List("cookie-value")), Full(xml.toString.toArray.map(_.toByte)), new HttpClient
      ) with PostListener {
       override def parameterVerifyer(httpMethod: PostMethod) ={
         shouldBeenCalled = true
         if(
            httpMethod.getParameter("field").getValue.equals("field value") &&
            httpMethod.getParameter("anotherField").getValue.equals("label value")
          )
           Unit
         else
           error("Value is not set correctly")
       }
      }

      response.submit((Label("field") -> "field value"), (Label("Label")->"label value"))
      shouldBeenCalled must be(true)
    }
  }

  def pass = true must be(true)
}





