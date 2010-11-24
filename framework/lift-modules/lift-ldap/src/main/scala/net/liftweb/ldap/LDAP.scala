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
package ldap {

import java.io.{InputStream, FileInputStream}
import java.util.{Hashtable, Properties}

import javax.naming.{AuthenticationException,CommunicationException,Context,NamingException}
import javax.naming.directory.{Attributes, BasicAttributes, SearchControls}
import javax.naming.ldap.{InitialLdapContext,LdapName}

import scala.collection.mutable.ListBuffer
import scala.collection.jcl.MapWrapper

import _root_.net.liftweb.util.{Props,SimpleInjector,ThreadGlobal}
import _root_.net.liftweb.common.{Box,Empty,Full,Loggable}

/**
 * Allow us to search and bind an username from a ldap server
 * To set the ldap server parameters override the parameters var directly:
 *    SimpleLDAPVendor.parameters = () => Map("ldap.username" -> ...
 * or set the parameters from a properties file or a inputStream
 *    SimpleLDAPVendor.parameters = () => SimpleLDAPVendor.parametersFromFile("/opt/config/ldap.properties")
 *    SimpleLDAPVendor.parameters = () => SimpleLDAPVendor.parametersFromStream(
                this.getClass().getClassLoader().getResourceAsStream("ldap.properties"))
 *
 * The mandatory parameters are :
 *  ldap.url -> LDAP Server url : ldap://localhost
 *  ldap.base -> Base DN from the LDAP Server : dc=company, dc=com
 *  ldap.userName -> LDAP user dn to perform search operations
 *  ldap.password -> LDAP user password
 *
 * It also can be initialized from boot with default Properties with setupFromBoot
 */
object SimpleLDAPVendor extends LDAPVendor {
    def parametersFromFile(filename: String) : Map[String, String] = {
        return parametersFromStream(new FileInputStream(filename))
    }

    def parametersFromStream(stream: InputStream) : Map[String, String] = {
        val p = new Properties()
        p.load(stream)

        return convertToStringMap(p.asInstanceOf[Hashtable[String, String]])
    }

    def setupFromBoot = {
      if(parameters.vend.isEmpty) {
        parameters.default.set(() => {
          Map("ldap.url"      -> Props.get("ldap.url").openOr(DEFAULT_URL),
              "ldap.base"     -> Props.get("ldap.base").openOr(DEFAULT_BASE_DN),
              "ldap.userName" -> Props.get("ldap.userName").openOr(DEFAULT_USER),
              "ldap.password" -> Props.get("ldap.password").openOr(DEFAULT_PASSWORD))
        })
      }
      
      // Set the test DN if provided
      Props.get("ldap.testLookup").foreach{ prop => testLookup.default.set(Full(prop))}
    }

    private def convertToStringMap(javaMap: Hashtable[String, String]) = {
        Map.empty ++ new MapWrapper[String, String]() {
            def underlying = javaMap
        }
    }
}

class LDAPVendor extends Loggable with SimpleInjector {
  // =========== Constants ===============
  final val DEFAULT_URL = "localhost"
  final val DEFAULT_BASE_DN = ""
  final val DEFAULT_USER = ""
  final val DEFAULT_PASSWORD = ""
  final val DEFAULT_INITIAL_CONTEXT_FACTORY = "com.sun.jndi.ldap.LdapCtxFactory"

  // =========== Configuration ===========

  /**
   * This can be set to test the InitialContext on each LDAP
   * operation. It should be set to a search DN.
   */
  val testLookup = new Inject[Box[String]](Empty){}

  /**
   * This sets the interval between connection attempts
   * on the InitialContext. The default is 5 seconds
   */
  val retryInterval = new Inject[Long](5000){}

  /**
   * This sets the maximum number of connection
   * attempts before giving up. The default is 6
   */
  val retryMaxCount = new Inject[Int](6){}

  /**
   * This sets the Directory SearchControls instance
   * that is used to refine searches on the provider.
   */
  val searchControls = new Inject[SearchControls](defaultSearchControls){}
  
  /**
   * The default SearchControls to use: search the
   * base DN with a sub-tree scope, and return the
   * "cn" attribute.
   */
  def defaultSearchControls() : SearchControls = {
    val constraints = new SearchControls()
    constraints.setSearchScope(SearchControls.SUBTREE_SCOPE)
    constraints.setReturningAttributes(Array("cn"))
    return constraints
  }

  /**
   * The Map of parameters to use for connecting to the
   * provider.
   */
  val parameters = new Inject[Map[String, String]](Map()){}

  // =========== Code ====================

  /**
   * Obtains a (possibly cached) InitialContext
   * instance based on the currently set parameters.
   */
  def initialContext = getInitialContext(parameters.vend)

  def attributesFromDn(dn: String): Attributes =
    initialContext.getAttributes(dn)

  /**
   * Searches the base DN for entities matching the given filter.
   */
  def search(filter: String): List[String] = {
    logger.debug("Searching for '%s'".format(filter))

    val resultList = new ListBuffer[String]()

    val searchResults = initialContext.search(parameters.vend.getOrElse("ldap.base", DEFAULT_BASE_DN),
                                              filter,
                                              searchControls.vend)

    while(searchResults.hasMore()) {
      resultList += searchResults.next().getName
    }
  
    return resultList.reverse.toList
  }

  /**
   * Attempts to authenticate the given DN against the configured
   * LDAP provider.
   */
  def bindUser(dn: String, password: String) : Boolean = {
    logger.debug("Attempting to bind user '%s'".format(dn))

    try {
      val username = dn + "," + parameters.vend.getOrElse("ldap.base", DEFAULT_BASE_DN)
      var ctx = openInitialContext(parameters.vend ++ Map("ldap.userName" -> username,
                                                       "ldap.password" -> password))
      ctx.close

      logger.debug("Successfully authenticated " + dn)
      true
    } catch {
      case ae : AuthenticationException => {
        logger.warn("Authentication failed for '%s' : %s".format(dn, ae.getMessage))
        false
      }
      case e: Exception => {
        logger.error("Error during LDAP authentication", e)
        false
      }
    }
  }

  // This caches the context for the current thread
  private[this] final val currentInitialContext = new ThreadGlobal[InitialLdapContext]()

  /**
   * This method attempts to fetch the cached InitialLdapContext for the
   * current thread. If there isn't a current context, open a new one. If a
   * test DN is configured, the connection (cached or new) will be validated
   * by performing a lookup on the test DN.
   */
  private def getInitialContext(props: Map[String, String]) : InitialLdapContext = {
    val maxAttempts = retryMaxCount.vend
    var attempts = 0

    var context : Box[InitialLdapContext] = Empty

    while (context.isEmpty && attempts < maxAttempts) {
      try {
        context = (currentInitialContext.box, testLookup.vend) match {
          // If we don't want to test an existing context, just return it
          case (Full(ctxt), Empty) => Full(ctxt)
          case (Full(ctxt), Full(test)) => {
            logger.debug("Testing InitialContext prior to returning")
            ctxt.lookup(test)
            Full(ctxt)
          }
          case (Empty,_) => {
            // We'll just allocate a new InitialContext to the thread
            currentInitialContext(openInitialContext(props))

            // Setting context to Empty here forces one more iteration in case a test
            // DN has been configured
            Empty
          }
        }
      } catch {
        case commE : CommunicationException => {
          logger.error("Communcations failure on attempt %d while " +
                       "verifying InitialContext: %s".format(attempts + 1, commE.getMessage))

          // The current context failed, so clear it
          currentInitialContext(null)

          // We sleep before retrying
          Thread.sleep(retryInterval.vend)
          attempts += 1
        }
      }
    }

    // We have a final check on the context before returning
    context match {
      case Full(ctxt) => ctxt
      case Empty => throw new NamingException("Gave up connecting to '%s' after %d attempts".
                                              format(props.get("ldap.url"), attempts))
    }
  }

  /**
   * This method does the actual work of setting up the environment and constructing
   * the InitialLdapContext.
   */
  private def openInitialContext (props : Map[String, String]) : InitialLdapContext = {
    logger.debug("Obtaining an initial context from '%s'".format(props.get("ldap.url")))
            
    var env = new Hashtable[String, String]()
    env.put(Context.PROVIDER_URL, props.getOrElse("ldap.url", DEFAULT_URL))
    env.put(Context.SECURITY_AUTHENTICATION, "simple")
    env.put(Context.SECURITY_PRINCIPAL, props.getOrElse("ldap.userName", DEFAULT_USER))
    env.put(Context.SECURITY_CREDENTIALS, props.getOrElse("ldap.password", DEFAULT_PASSWORD))
    env.put(Context.INITIAL_CONTEXT_FACTORY, parameters().getOrElse("ldap.initial_context_factory",
                                                                    DEFAULT_INITIAL_CONTEXT_FACTORY))
    new InitialLdapContext(env, null)
  }
}

}
}
