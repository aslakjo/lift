package net.liftweb.http.testing.matchers

import _root_.org.specs._
import _root_.org.specs.matcher.Matcher
import net.liftweb.http.testing._
import scala.xml._
import net.liftweb.common._

class TestKitXmlMatcherTests extends Specification{
  val reportError = new net.liftweb.http.testing.ReportFailure {
    def fail(msg: String): Nothing = error(msg)
  }

  val successReporter = new net.liftweb.http.testing.ReportFailure {
    def fail(msg: String): Nothing = throw new Success
  }

  "Test kit matcher " should {

    "match a single node" in{
      implicit val error = reportError
      val response : Box[Elem] = Full(<html>
        <body>
          <h1>test</h1>
        </body>
      </html>)

      val matcher = new TestKitXmlMatcher(response)
      matcher.should_contain(<h1/>)
      pass
    }

    "match a single node with attribute" in{
      implicit val error = reportError
      val response : Box[Elem] = Full(<html>
        <body>
          <h1 class="big">test</h1>
        </body>
      </html>)

      val matcher = new TestKitXmlMatcher(response)
      matcher.should_contain(<h1 class="big"/>)
      pass
    }

    "not match node if it does not exists" in{
      implicit val reportError = successReporter
      val response : Box[Elem] = Full(<html>
        <body>
          <h1 class="big">test</h1>
        </body>
      </html>)

      val matcher = new TestKitXmlMatcher(response)
      try{
        matcher.should_contain(<h2 class="big"/>)
        error("Should not match")
      }catch{
        case x:Success => pass
      }
    }

    "not match node if it's attribute does not exists" in{
      implicit val reportError = successReporter
      val response : Box[Elem] = Full(<html>
        <body>
          <h1 class="large">test</h1>
        </body>
      </html>)

      val matcher = new TestKitXmlMatcher(response)

      try
      {
        matcher.should_contain(<h1 class="big"/>)
        error("Should not match")
      } catch {
        case x:Success => pass
      }
    }
  }

  def pass = true must be(true)
}

