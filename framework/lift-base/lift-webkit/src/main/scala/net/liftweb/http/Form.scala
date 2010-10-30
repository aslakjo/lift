/*
 * Copyright 2007-2010 WorldWide Conferencing, LLC
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
package http {

import common._
import util._
import scala.xml.{MetaData, Elem, NodeSeq, UnprefixedAttribute, Null, Text}

/**
 * An alternative to SHtml for form element generation.  Rather than
 * having an instance variables floating around, the FormElem vended
 * from LiftForm keep their own state internally.
 */
object LiftForm {
  /**
   * Vend an HTML text input
   */
  def formText[T](value: T, params: FormParam[T]*)(implicit toFrom: ToFrom[T]): FormElem[T] = new FormElem(value, params.toList, toFrom) {
    def strToForm(str: String): NodeSeq = 
      <input type="input" value={str} name={id}/>
  }

  /**
   * Vend a textarea
   */
  def formTextarea[T](value: T, params: FormParam[T]*)(implicit toFrom: ToFrom[T]): FormElem[T] = new FormElem(value, params.toList, toFrom) {
    def strToForm(str: String): NodeSeq = 
      <textarea name={id}>{str}</textarea>
  }

  /**
   * Vend a password field
   */
  def formPassword[T](value: T, params: FormParam[T]*)(implicit toFrom: ToFrom[T]): FormElem[T] = new FormElem(value, params.toList, toFrom) {
    def strToForm(str: String): NodeSeq = 
      <input type="password" value={str} name={id}/>
  }

  /**
   * The parameters for form elements.  There are lots of implicit
   * conversions so generally, the right thing happens
   */
  sealed trait FormParam[-T]
  
  /**
   * Attributes to be added the the generated for elements
   */
  final case class AttributeParam(metaData: MetaData) extends FormParam[Any]

  /**
   * Filters that transform the element
   */
  final case class FilterParam[T](f: Box[T] => Box[T]) extends FormParam[T]

  /**
   * Implicits for the FormParam
   */
  object FormParam {
    implicit def strPairToAttributeParam(pair: (String, String)): FormParam[Any] = AttributeParam(new UnprefixedAttribute(pair._1, pair._2, Null))
      
    implicit def boxBoxFuncToFormParam[T](f: Box[T] => Box[T]): FormParam[T] = 
      FilterParam(f)

    implicit def tBoxFuncToFormParam[T](f: T => Box[T]): FormParam[T] = 
      FilterParam((p: Box[T]) => p.flatMap(f))

    implicit def funcToFormParam[T](f: T => T): FormParam[T] = 
      FilterParam((p: Box[T]) => p.map(f))

    implicit def tFieldError[T](f: T => List[FieldError]): FormParam[T] = 
      FilterParam((p: Box[T]) => p.flatMap(v => f(v) match {
        case Nil => Full(v)
        case x :: Nil => Failure(x.msg.text, Empty, Empty)
        case xs => new ParamFailure("Multiple Failures", Empty, Empty,
                                    FailureList(xs.map(x => Failure(x.msg.text, Empty, Empty))))
      }))
  }

  def removeRegExChars(regEx: String): String => String =
  s => s match {
    case null => null
    case s => s.replaceAll(regEx, "")
  }

  def toLower: String => String =
  s => s match {
    case null => null
    case s => s.toLowerCase
  }

  def toUpper: String => String =
  s => s match {
    case null => null
    case s => s.toUpperCase
  }

  def trim: String => String =
  s => s match {
    case null => null
    case s => s.trim
  }

  def notNull: String => String =
  s => s match {
    case null => ""
    case x => x
  }

  /**
   * A validation helper.  Make sure the string is at least a particular
   * length and generate a validation issue if not
   */
  def valMinLen(len: => Int, msg: => String): String => List[FieldError] =
  s => s match {
    case str if (null ne str) && str.length >= len => Nil
    case _ => List(FieldError(new FieldIdentifier{}, Text(msg)))
  }


  /**
   * A validation helper.  Make sure the string is no more than a particular
   * length and generate a validation issue if not
   */
  def valMaxLen(len: => Int, msg: => String): String => List[FieldError] =
  s => s match {
    case str if (null ne str) && str.length <= len => Nil
    case _ => List(FieldError(new FieldIdentifier{}, Text(msg)))
  }

  /**
   * Make sure the field matches a regular expression
   */
  def valRegex(pat: => java.util.regex.Pattern, msg: => String): String => List[FieldError] =
  s => s match {
    case str if (null ne str) && pat.matcher(str).matches => Nil
    case _ => List(FieldError(new FieldIdentifier{}, Text(msg)))
  }

  abstract class FormElem[T](value: T, 
                    params: List[FormParam[T]],
                    toFrom: ToFrom[T]) extends Boxable[T] with Bindable with BaseField {

    type ValueType = Box[T]

    def name: String = "" // FIXME

    lazy val id: String = Helpers.nextFuncName
    
    private object curVal extends 
    TransientRequestVar[Box[T]](calculateValueFromRequest) {
      override def __nameSalt = id
    }

    def set(value: Box[T]): Box[T] = {curVal.set(value); value}

    def get: Box[T] = curVal.is
    def is: Box[T] = get
    
    protected def calculateValueFromRequest: Box[T] = 
      filters.foldLeft(toFrom.fromStringList(S.params(id))) {
        case (v, f) => f(v)
      }

    curVal.set(Full(value))

    def asBox: Box[T] = curVal.is
    
    def filters: List[Box[T] => Box[T]] = params.flatMap {
      case f: FilterParam[T] => List(f.f)
      case _ => Nil
    }
    
    protected def addAttrs(in: Elem): Elem =
      params.flatMap {
        case a: AttributeParam => List(a)
        case _ => Nil
      }.foldLeft(in)(_ % _.metaData)
    
    /**
     * Give the string representation, create a form element
     * // FIXME --- really needs to be generic
     */
    protected def strToForm(str: String): NodeSeq

    def validate: List[FieldError] = Nil // FIXME do validate

    def validations: List[ValueType => List[FieldError]] = Nil // FIXME

  /**
   * A list of functions that transform the value before it is set.  The transformations
   * are also applied before the value is used in a query.  Typical applications
   * of this are trimming and/or toLowerCase-ing strings
   */
  protected def setFilter: List[ValueType => ValueType] = Nil // FIXME



    def toForm: Box[NodeSeq] = 
      (for {
        cv <- curVal.is
        str <- toFrom.makeString(cv)
      } yield strToForm(str) match {
        case e: Elem => e
        case x => x
      })
  }

  /**
   * Form elements can take different types (e.g., String, Date, etc.)
   * ToFrom is the conversion between the type and what the forms need
   */
  trait ToFrom[T] {
    def makeString(in: T): Box[String]
    def fromStringList(lst: List[String]): Box[T]
  }

  object ToFrom {
    implicit lazy val toFromString: ToFrom[String] = 
      new ToFrom[String] {
        def makeString(in: String): Box[String] = Box !! in
        def fromStringList(lst: List[String]): Box[String] =
          lst match {
            case Nil => Failure("Got an empty list from the server",
                                Empty, Empty)
            case x :: _ => Full(x)
          }
      }

    implicit lazy val toFromInt: ToFrom[Int] = 
      new ToFrom[Int] {
        def makeString(in: Int): Box[String] = Full(in.toString)
        def fromStringList(lst: List[String]): Box[Int] =
          lst match {
            case Nil => Failure("Got an empty list from the server",
                                Empty, Empty)
            case x :: _ => Helpers.asInt(x) ?~ "Invalid Number"
          }
      }
  }

  class EnhancedCssSelector(selector: String) {
    def #=> (unitFunc: () => Any): CssBind = {
      import Helpers._
      def wrapIt(in: NodeSeq): NodeSeq = {
        import S._

        val funcHolder: AFuncHolder = unitFunc
        fmapFunc(funcHolder){
          funcName =>
            in.map {
              case e: Elem => e % new UnprefixedAttribute("name", funcName, Null)
              case x => x
            }
        }
      }


      selector #> wrapIt _
    }
  }

  implicit def strToEnhancedCssSelector(sel: String): EnhancedCssSelector =
    new EnhancedCssSelector(sel)
}

}
}
