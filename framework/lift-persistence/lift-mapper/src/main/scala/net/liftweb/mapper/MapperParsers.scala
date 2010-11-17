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
package mapper {

import common.{Box,Full}
import http.S
import util.BasicTypesHelpers

/**
 * This trait provides configuration points for global parse handling. The default
 * implementations of all parsers conforms to pre-MapperParsers behavior.
 *
 * TODO: More docs
 */
trait MapperParsers {
  def parseInt(value : String, field: MappedField[Int, _]) : Int = {
    field.set(BasicTypesHelpers.toInt(value))
  }

  def parseLong(value : String, field: MappedField[Long, _]) : Long = {
    field.set(BasicTypesHelpers.toLong(value))
  }

  /**
   * Override this method to set your own global error handling. The default
   * is to do nothing on a parse failure.
   */
  def parseErrorHandler[T](value : String, field : MappedField[T,_]) : T =
    field.is

  protected def dispatchErrorHandling[T](value : String, field : MappedField[T,_]) : T =
    field.parseErrorHandler match {
      case Full(handler) => handler(value)
      case _ => parseErrorHandler(value,field)
    }
}

object DefaultMapperParsers extends MapperParsers

object NewMapperParsers extends MapperParsers {
  def tryParse[T](value : String, field : MappedField[T,_], default : String => T) : T =
    try {
      field.set((field.parseValue openOr default)(value))
    } catch {
      case _ => dispatchErrorHandling(value, field)
    }

  override def parseInt(value : String, field: MappedField[Int, _]) : Int =
    tryParse(value, field, java.lang.Integer.parseInt)

  override def parseLong(value : String, field: MappedField[Long, _]) : Long =
    tryParse(value, field, java.lang.Long.parseLong)

  override def parseErrorHandler[T](value : String, field: MappedField[T,_]) : T = {
    S.error("Invalid value \"%s\" for %s".format(value, field.displayName))
    field.is
  }
}

}} // Close nested packages
