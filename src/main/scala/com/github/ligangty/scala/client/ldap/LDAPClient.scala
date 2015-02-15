package com.github.ligangty.scala.client.ldap

import java.util.Properties
import javax.naming.{Context, NamingEnumeration, NamingException}
import javax.naming.directory.{Attributes, SearchControls, SearchResult}
import javax.naming.ldap.{InitialLdapContext, LdapContext}

import org.apache.commons.lang3.StringUtils
import org.slf4j.{Logger, LoggerFactory}

/**
 * Created by gli on 2/15/15.
 */
class LDAPClient private() {
  private val LOGGER: Logger = LoggerFactory.getLogger(classOf[LDAPClient])

  private val PROVIDER_URL = "ldap.provider.url"


  private val DEFAULT_PROVIDER_URL = "ldap://ldap.yourdomain.com"
  private val DEFAULT_PROVIDER_SSL_URL = "ldaps://ldap.yourdomain.com:636"
  private val DEFAULT_USER_SEARCH_BASE = "ou=users,dc=yourdomain,dc=com"
  private val DEFAULT_GROUP_SEARCH_BASE = "ou=groups,dc=yourdomain,dc=com"
  private val DEFAULT_USER_GROUP_SEARCH_BASE = DEFAULT_GROUP_SEARCH_BASE
  private val DEFAULT_USER_FILTER = "(&(objectClass=person)(uid={0}))"
  private val DEFAULT_GROUP_FILTER = "(&(objectClass=posixGroup)(cn={0}))"
  private val DEFAULT_USER_GROUPS_FILTER = "(&(objectclass=posixGroup)(memberuid={0}))"
  private val DEFAULT_USER_IN_GROUP_FILTER = "(&(objectclass=posixGroup)(memberUid={0})(cn={1}))"
  private val DEFAULT_SECURITY_PRINCIPAL = ""
  private val DEFAULT_SECURITY_PASSWORD = ""

  private val config: Properties = new Properties()


  def this(ldapServerUrl: String) = {
    this()
    config.put(PROVIDER_URL, ldapServerUrl)
  }

  def findPersonByName(kerberosName: String): LDAPPerson = {
    var ctx: InitialLdapContext = null
    var person: LDAPPerson = null
    try {
      LOGGER.debug("start looking up {} user entry in LDAP", kerberosName)
      ctx = buildInitialLdapContext()
      val userCtx = DEFAULT_USER_SEARCH_BASE
      val userFilter =  DEFAULT_USER_FILTER
      val result = getSearchResults(ctx, userCtx, userFilter, kerberosName)
      if (result.hasMore()) {
        val sr = result.next()
        // make sure there is not another item available, there should be
        // only 1 match
        if (result.hasMoreElements()) {
          throw new LDAPClientException("has more than one user for kerberos name " + kerberosName)
        }
        person = extractLDAPPerson(sr)
        LOGGER.debug("Entry in LDAP found and result of matching with given user id is {}", kerberosName)
      }
      result.close()

    } catch {
      case e: NamingException => throw new LDAPClientSystemException("some error occured when fetching person", e)
    } finally {
      closeContext(ctx)
    }

    return person
  }

  private def getSearchResults(ctx: LdapContext, contextBase: String, searchfilter: String,
                               searchingElement: String): NamingEnumeration[SearchResult] = {
    val DEFAULT_FILTER_REPLACE_PATTERN = "\\{0\\}"
    val filter = searchfilter.replaceAll(DEFAULT_FILTER_REPLACE_PATTERN, searchingElement)
    LOGGER.debug("Seaching for existence with filter {} on contextBase {}", Array(filter, contextBase))
    val constraints = new SearchControls
    constraints.setSearchScope(SearchControls.SUBTREE_SCOPE)
    return ctx.search(contextBase, filter, constraints)
  }

  private def extractLDAPPerson(personSearchResult: SearchResult): LDAPPerson = {
    val person = new LDAPPerson()

    val personAttributes = personSearchResult.getAttributes()

    val userAttrId = LDAPAttributes.PersonAttributes.ATTR_UID

    person.userName = getAttributeValue(personAttributes, userAttrId)

    val ldapGivenName: String = getAttributeValue(personAttributes, LDAPAttributes.PersonAttributes.ATTR_GIVEN_NAME)
    val ldapSN: String = getAttributeValue(personAttributes, LDAPAttributes.PersonAttributes.ATTR_SN)
    person.realName = ldapGivenName + " " + ldapSN

    person.email = getAttributeValue(personAttributes, LDAPAttributes.PersonAttributes.ATTR_MAIL)

    person.managerUsername = getUidFromDN(getAttributeValue(personAttributes, LDAPAttributes.PersonAttributes.ATTR_MANAGER))

    var ldapJobTitle: String = getAttributeValue(personAttributes, LDAPAttributes.PersonAttributes.ATTR_RHAT_JOB_TITLE)
    // because someone does not have "rhatjobtitle" attribute, so use "title" as jobtitle if first is null
    if (StringUtils.isEmpty(ldapJobTitle)) {
      ldapJobTitle = getAttributeValue(personAttributes, LDAPAttributes.PersonAttributes.ATTR_TITLE)
    }
    person.jobTitle = ldapJobTitle

    person.costCenter = getAttributeValue(personAttributes, LDAPAttributes.PersonAttributes.ATTR_RHAT_COST_CENTER)

    person.department = getAttributeValue(personAttributes, LDAPAttributes.PersonAttributes.ATTR_RHAT_COST_CENTER_DESC)

    person.employeeNumber =getAttributeValue(personAttributes, LDAPAttributes.PersonAttributes.ATTR_EMPLOYEE_NUMBER)

    person.employeeType = getAttributeValue(personAttributes, LDAPAttributes.PersonAttributes.ATTR_EMPLOYEE_TYPE)

    person.oracleId = getAttributeValue(personAttributes, LDAPAttributes.PersonAttributes.ATTR_RHAT_ORACLE_PERSON_ID)

    person.location = getAttributeValue(personAttributes, LDAPAttributes.PersonAttributes.ATTR_RHAT_LOCATION)

    person.hireDate = getAttributeValue(personAttributes, LDAPAttributes.PersonAttributes.ATTR_RHAT_HIRE_DATE)

    return person
  }

  private def getAttributeValue[T](attributes: Attributes, attrId: String): T = {
    val attr = attributes.get(attrId)
    if (attr == null) {
      LOGGER.debug("Ldap attribute value is missing for attribute " + attrId)
      return Option(null).get.asInstanceOf[T]
    }
    return attr.get().asInstanceOf[T]
  }


  private def getUidFromDN(dn: String): String = {
    val userAttrId = LDAPAttributes.PersonAttributes.ATTR_UID
    if (StringUtils.isNotBlank(dn)) {
      val startIndex = dn.trim().indexOf(userAttrId + "=") + userAttrId.length() + 1
      val endIndex = dn.trim().indexOf(',', startIndex)
      return dn.trim().substring(startIndex, endIndex)
    }
    return null
  }

  private def buildInitialLdapContext(): InitialLdapContext = {

    // Set defaults for key values if they are missing
    var factoryName = this.config.getProperty(Context.INITIAL_CONTEXT_FACTORY)

    if (StringUtils.isBlank(factoryName)) {
      factoryName = "com.sun.jndi.ldap.LdapCtxFactory"
      this.config.setProperty(Context.INITIAL_CONTEXT_FACTORY, factoryName)
    }

    val authType = this.config.getProperty(Context.SECURITY_AUTHENTICATION)

    if (StringUtils.isBlank(authType)) {
      this.config.setProperty(Context.SECURITY_AUTHENTICATION, "simple")
    }

    val protocol = this.config.getProperty(Context.SECURITY_PROTOCOL)

    var providerURL = this.config.getProperty(Context.PROVIDER_URL)
    if (StringUtils.isBlank(providerURL)) {
      providerURL = this.config.getProperty(PROVIDER_URL)
      if (StringUtils.isBlank(providerURL)) {
        // set default provider URL to redhat internal one if no provider supplied in outer settings, but not
        // recommended
        providerURL = if ("ssl".equalsIgnoreCase(protocol)) DEFAULT_PROVIDER_SSL_URL else DEFAULT_PROVIDER_URL
      }
      this.config.setProperty(Context.PROVIDER_URL, providerURL)
    }

    val binduser = DEFAULT_SECURITY_PRINCIPAL

    if (StringUtils.isNotBlank(binduser)) {
      this.config.setProperty(Context.SECURITY_PRINCIPAL, binduser)
    }

    val bindpwd = DEFAULT_SECURITY_PASSWORD

    if (StringUtils.isNotBlank(binduser)) {

      this.config.setProperty(Context.SECURITY_CREDENTIALS, bindpwd)
    }

    if (LOGGER.isDebugEnabled()) {
      LOGGER.debug("Using following InitialLdapContext properties:")
      LOGGER.debug("Factory {}", this.config.getProperty(Context.INITIAL_CONTEXT_FACTORY))
      LOGGER.debug("Authentication {}", this.config.getProperty(Context.SECURITY_AUTHENTICATION))
      LOGGER.debug("Protocol {}", this.config.getProperty(Context.SECURITY_PROTOCOL))
      LOGGER.debug("Provider URL {}", this.config.getProperty(PROVIDER_URL))
    }

    return new InitialLdapContext(this.config, null)
  }

  private def closeContext(ctx: LdapContext): Unit = {
    if (ctx != null) {
      try {
        ctx.close()
      } catch {
        case e: NamingException => throw new LDAPClientSystemException("some error occured when close ldap client", e)
      }
    }
  }

}
