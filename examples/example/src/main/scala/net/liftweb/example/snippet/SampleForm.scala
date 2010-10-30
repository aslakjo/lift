/*
 * Copyright 2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb {
package example {
package snippet {

import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.common._
import Helpers._

class SampleForm extends StatefulSnippet {
  import LiftForm._ 
  import HLists._
  import CombinableBox._

  def dispatch = {
    case _ => render
  }

  private val whence = S.referer

  private val name = formText("", valMinLen(3, "Your name must be 3 characters"))
  private val age = formText(0, 
                             (i: Int) => Full(i).filter(_ > 12) ?~
                             "You must be at least 12")

  private def checkIt() {
    (for {
      name :+: age :+: _ <- name :&: age
    } yield {
      S.notice("Name: "+name+" name.length "+name.length+" age "+age)
      S.redirectTo(whence openOr "/")
    }) match {
      case f: Failure => S.error(f)
      case _ =>
    }
  }
 
  def render = {
    "#name" #> name & "#age" #> age & "#submit" #=> checkIt _
  }
}

}
}
}
