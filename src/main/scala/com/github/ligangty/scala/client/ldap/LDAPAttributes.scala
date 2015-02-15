package com.github.ligangty.scala.client.ldap

/**
 * Created by gli on 2/15/15.
 */
object LDAPAttributes {
  object PersonAttributes {
    val ATTR_UID = "uid";
    val ATTR_GIVEN_NAME = "givenName";
    val ATTR_SN = "sn";
    val ATTR_MAIL = "mail";
    val ATTR_MANAGER = "manager";
    val ATTR_RHAT_JOB_TITLE = "rhatJobTitle";
    val ATTR_TITLE = "title";
    val ATTR_RHAT_COST_CENTER = "rhatCostCenter";
    val ATTR_RHAT_COST_CENTER_DESC = "rhatCostCenterDesc";
    val ATTR_EMPLOYEE_NUMBER = "employeeNumber";
    val ATTR_EMPLOYEE_TYPE = "employeeType";
    val ATTR_RHAT_ORACLE_PERSON_ID = "rhatOraclePersonID";
    val ATTR_RHAT_LOCATION = "rhatLocation";
    val ATTR_RHAT_HIRE_DATE = "rhatHireDate";
  }
  object PosixGroupAttributes {
    val ATTR_CN = "cn";
    val ATTR_GID_NUMBER = "gidNumber";
    val ATTR_MEMBER_UID = "memberUid";
  }
}
