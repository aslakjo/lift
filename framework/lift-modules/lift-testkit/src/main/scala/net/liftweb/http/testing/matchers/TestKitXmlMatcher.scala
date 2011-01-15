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
       case _ => ;
    }
  }

  protected def filter(xml:Elem, element:Elem):Option[NodeSeq]= {
    var matchingTag = xml \\ element.label

    if(matchingTag.isEmpty)
      None
    else
      matchAttributes(matchingTag, element.attributes)
  }

  protected def matchAttributes(tag:NodeSeq, attributes:MetaData):Option[NodeSeq] ={
    val matches = for( (attr,value) <- attributes.asAttrMap
                      if ( (tag \\ new String("@" + attr)).text.equals(new String(value)))
                  )
                  yield tag

    if(attributes.isEmpty)
      Some(tag)
    else if(matches.isEmpty)
      None
    else
      Some(tag)
  }

  protected def isResponseValid= !responseBox.isEmpty
}