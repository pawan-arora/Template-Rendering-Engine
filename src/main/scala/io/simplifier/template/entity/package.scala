package io.simplifier.template

package object entity {

  case class Parameter(name: String, alias: Option[String], constValue: Option[String], dataTypeId: String, optional: Boolean, description: Option[String])

}
