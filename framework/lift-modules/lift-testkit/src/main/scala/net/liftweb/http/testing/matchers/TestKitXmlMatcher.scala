package net.liftweb.http.testing.matchers

import scala.xml._
import net.liftweb.common.Box
import net.liftweb.http.testing._

object TestKitXmlMatchers
{
  implicit def xml2XmlMatcher(box :Box[scala.xml.Elem])(implicit reportFailure: ReportFailure):TestKitXmlMatcher = new TestKitXmlMatcher(box)
}

class TestKitXmlMatcher(val responseBox:Box[scala.xml.Elem] )(implicit reportFailure: ReportFailure){
  def should_contain(element:Elem)={
    if(isResponseValid)
    {
      containsElement(element)
    }else
      reportFailure.fail("No response was returned:" + responseBox )
  }

  protected def containsElement(element:Elem): Unit = {
     filter(responseBox.get, element)  match {
       case None => reportFailure.fail("Response '" + responseBox + "' did not match '" + element + "'")
        case _ =>;
    }
  }

  protected def filter(xml:Elem, element:Elem):Option[NodeSeq]= {

    var remainding = xml  \\ element.label

    val attributes = element.attributes.asAttrMap
    if(attributes.size > 0 )
    {
      val attribute = remainding \\ ("@" + attributes.head._1)
      if(attribute.isEmpty)
        None
      else
        remainding \\ ("@" + attributes.head._1) find {i=> true}
    }
    Some(remainding)

  }

  protected def isResponseValid= !responseBox.isEmpty
}