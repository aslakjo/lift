/*
* Copyright 2010 WorldWide Conferencing, LLC
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/

package net.liftweb {
package mongodb {

import common.{Box, Failure, Full}
import json.{Formats, Extraction, JsonAST}
import JsonAST.{JObject, JValue}

import java.util.UUID
import org.bson.types.ObjectId

/*

sealed abstract class MongoRef {
  def id: Any
  def ref: String
  implicit def formats: Formats

  def asJValue: JValue = MongoRef.asJValue(this, formats)
  def fromJValue(jv: JValue): Box[MongoRef] = jv match {
    case jo: JObject =>
      try {
        Full(jo.extract[MongoRef])
      }
      catch {
        case e: Exception => Failure(e.getMessage)
      }
    case other => Failure("Expected a JObject, not a " + (if (other == null) "null" else other.getClass.getName))
  }
}
object MongoRef {
  def apply(id: Any, ref: String)(implicit formats: Formats) = id match {
    case oid: ObjectId => ObjectIdRef(oid, ref)(formats)
    case uid: UUID => UUIDRef(uid, ref)(formats)
    case s: String => StringRef(s, ref)(formats)
    case i: Int => IntRef(i, ref)(formats)
    case l: Long => LongRef(l, ref)(formats)
  }

  def asJValue(inst: MongoRef, formats: Formats): JValue = Extraction.decompose(inst)(formats)
}
case class ObjectIdRef(id: ObjectId, ref: String)(implicit fmts: Formats) extends MongoRef {
  def formats = fmts
}
case class UUIDRef(id: UUID, ref: String)(implicit fmts: Formats) extends MongoRef {
  def formats = fmts
}
case class StringRef(id: String, ref: String)(implicit fmts: Formats) extends MongoRef {
  def formats = fmts
}
case class IntRef(id: Int, ref: String)(implicit fmts: Formats) extends MongoRef {
  def formats = fmts
}
case class LongRef(id: Long, ref: String)(implicit fmts: Formats) extends MongoRef {
  def formats = fmts
}
*/
}
}
