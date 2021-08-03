package io.simplifier.template.params

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import com.google.gson.Gson
import spray.json.DefaultJsonProtocol

trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val requestApp = jsonFormat8(Parameters)
}
case class Parameters(invitationUrl: Option[String] = None, fullName: Option[String] = None,
                      userId: Option[String] = None, userLoginName: Option[String] = None,
                      instanceURL: Option[String] = None, email:Option[String] = None,
                      appName:Option[String] = None, recoverLink: Option[String] = None) {
  def jsonString: String = {
    val fullName = this.fullName.getOrElse("")
    val userId = this.userId.getOrElse("")
    val userLoginName = this.userLoginName.getOrElse("")
    val instanceURL = this.instanceURL.getOrElse("")
    val emailId = this.email.getOrElse("")
    val appName = this.appName.getOrElse("")
    val invitationURL = this.invitationUrl.getOrElse("")
    val recoverLink = this.recoverLink.getOrElse("")
    s"""{"fullName": "$fullName", "userId": "$userId", "userLoginName": "$userLoginName", "instanceURL": "$instanceURL", "email": "$emailId","appName": "$appName", "invitationUrl": "$invitationURL", "RecoverLink": "$recoverLink"}""".stripMargin
  }
}


