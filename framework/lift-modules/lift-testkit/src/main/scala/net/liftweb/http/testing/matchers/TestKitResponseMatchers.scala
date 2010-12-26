package net.liftweb.http.testing.matchers

import scala.xml._
import net.liftweb.common.Box
import net.liftweb.http.testing._
import org.apache.commons.httpclient.HttpClient


object TestKitResponseMatchers
{
  implicit def xml2ResponseMatcher(response :net.liftweb.http.testing.HttpResponse)(implicit reportFailure: ReportFailure):TestKitResponseMatcher = new TestKitResponseMatcher(response)
}

class TestKitResponseMatcher(response : net.liftweb.http.testing.HttpResponse) (implicit reportFailure: ReportFailure){

  def should_be_redirect(To:String) {
    isRedirect

    response.headers("Location") match {
      case To :: Nil =>  Unit
      case _ => reportFailure.fail("Redirect should have gone to " + To + " but didnt.")
    }
  }

  def should_be_redirect() = isRedirect

  private def isRedirect ={
    if(response.code != 302)
      reportFailure.fail("Response is not a redirect. Code is " + response.code + " should have been 302.")
  }
}