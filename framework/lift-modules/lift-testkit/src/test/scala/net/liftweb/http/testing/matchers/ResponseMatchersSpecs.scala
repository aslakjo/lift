package net.liftweb.http.testing.matchers

import _root_.net.liftweb.http.testing.HttpResponse
import _root_.org.specs._
import _root_.org.specs.matcher.Matcher
import net.liftweb.common.{Box, Full}
import _root_.org.apache.commons.httpclient._
import methods._

class TestKitResponseMatchersSpec extends Specification{


  val reportFailure: net.liftweb.http.testing.ReportFailure = new net.liftweb.http.testing.ReportFailure {
    def fail(msg: String): Nothing = error(msg)
  }

  "Upon receiveing a response" should {
    "detect a redirect" in{
      implicit val error = reportFailure 
      val response = new HttpResponse ( "url", 302, "message", Map("Set-Cookie" -> List("cookie")),
        Full(<html></html>.toString.toArray.map(_.toByte)), new HttpClient
      )

      new TestKitResponseMatcher(response).should_be_redirect
      pass
    }

    "detect a redirect via implicit" in{
      import net.liftweb.http.testing.matchers.TestKitResponseMatchers._
      implicit val error = reportFailure
      val response = new HttpResponse ( "url", 302, "message", Map("Set-Cookie" -> List("cookie")),
        Full(<html></html>.toString.toArray.map(_.toByte)), new HttpClient
      )

      response.should_be_redirect
      pass
    }

    "detect a normal page as not a redirect" in{
      val response = new HttpResponse ( "url", 200, "message", Map("Set-Cookie" -> List("cookie")),
        Full(<html></html>.toString.toArray.map(_.toByte)), new HttpClient
      )

      implicit val expectingFailure = new net.liftweb.http.testing.ReportFailure {
        def fail(msg: String): Nothing = throw new Success
      }

      try{
        new TestKitResponseMatcher(response).should_be_redirect
        error("A redirect should been detected")
      }catch{
        case x:Success => pass
      }
    }

    "detect the redirect localtion" in {
      implicit val error = reportFailure
      val response = new HttpResponse ( "url", 302, "message",
        Map("Location" -> List("/redirect")),
        Full(<html></html>.toString.toArray.map(_.toByte)), new HttpClient
      )

      new TestKitResponseMatcher(response).should_be_redirect("/redirect")
      pass
    }

    "detect a wrong redirect location" in {
      val response = new HttpResponse ( "url", 302, "message", 
        Map("Location" -> List("/redirect")),
        Full(<html></html>.toString.toArray.map(_.toByte)), new HttpClient
      )

      implicit val expectingFailure = new net.liftweb.http.testing.ReportFailure {
        def fail(msg: String): Nothing = throw new Success
      }

      try{
        new TestKitResponseMatcher(response).should_be_redirect("/another/location")
        error("A wrong redirect should have been detected")
      }catch{
        case x:Success => pass
      }
    }

  }
  def pass = true must be(true)
}

case class Success extends Throwable
