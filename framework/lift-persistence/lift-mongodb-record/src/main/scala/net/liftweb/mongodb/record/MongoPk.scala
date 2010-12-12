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
package mongodb {
package record {

import field._
import common.{Box, Empty, Full}
import util.StringHelpers.randomString

import scala.util.Random
import java.util.UUID

import org.bson.types.ObjectId
import net.liftweb.record.field.{IntField, LongField, StringField}

/*
 * Trait for creating a "Primary Key" Field. All Mongo Records must have
 * a _id defined. These all have an id field that is saved as _id in the
 * database.
 */
trait MongoPk[PkType <: MongoField] {
  def id: PkType
  /** Override this to set default value of id field */
  def defaultIdValue: Any
}

trait ObjectIdPk[OwnerType <: MongoRecord[OwnerType]]
  extends MongoPk[ObjectIdField[OwnerType] with MongoField]
{
  self: OwnerType =>

  def defaultIdValue = ObjectId.get

  object id extends ObjectIdField(this.asInstanceOf[OwnerType]) with MongoField {
    override def mongoName = Full("_id") // store this as _id in the database
    override def defaultValue = defaultIdValue
  }
}

trait UUIDPk[OwnerType <: MongoRecord[OwnerType]]
  extends MongoPk[UUIDField[OwnerType] with MongoField]
{
  self: OwnerType =>

  def defaultIdValue = UUID.randomUUID

  object id extends UUIDField(this.asInstanceOf[OwnerType]) with MongoField {
    override def mongoName = Full("_id") // store this as _id in the database
    override def defaultValue = defaultIdValue
  }
}

trait StringPk[OwnerType <: MongoRecord[OwnerType]]
  extends MongoPk[StringField[OwnerType] with MongoField]
{
  self: OwnerType =>

  def defaultIdValue = randomString(32)

  object id extends StringField(this.asInstanceOf[OwnerType], 12) with MongoField {
    override def mongoName = Full("_id") // store this as _id in the database
    override def defaultValue = defaultIdValue
  }
}

trait IntPk[OwnerType <: MongoRecord[OwnerType]]
  extends MongoPk[IntField[OwnerType] with MongoField]
{
  self: OwnerType =>

  def defaultIdValue = Random.nextInt

  object id extends IntField(this.asInstanceOf[OwnerType]) with MongoField {
    override def mongoName = Full("_id") // store this as _id in the database
    override def defaultValue = defaultIdValue
  }
}

trait LongPk[OwnerType <: MongoRecord[OwnerType]]
  extends MongoPk[LongField[OwnerType] with MongoField]
{
  self: OwnerType =>

  def defaultIdValue = Random.nextLong

  object id extends LongField(this.asInstanceOf[OwnerType]) with MongoField {
    override def mongoName = Full("_id") // store this as _id in the database
    override def defaultValue = defaultIdValue
  }
}

}
}
}
