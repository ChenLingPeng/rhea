package controllers

import be.objectify.deadbolt.scala.DeadboltActions
import controllers.Application._
import models.{User, EmailSet}
import play.api.libs.json.Json
import play.api.mvc.{Action, Controller}
import security.MyDeadboltHandler

/**
 * Created by zhaorui on 2014/11/10.
 */
object EmailSetApp extends Controller with DeadboltActions{

  def setEmail() = SubjectPresent(new MyDeadboltHandler(failResult = Ok(views.html.neonlogin(loginForm)))) {
    Action { implicit request =>
      val user = User.getUserById(request.session.get("session.id").get.toLong)
      var send = ""
      var carbonCopy = ""
      var blindCarbonCopy = ""
      if(request.body.asFormUrlEncoded.get("sendnonempty")(0).toInt == 1){
         send = request.body.asFormUrlEncoded.get("sendemail[]").toList.mkString(";")
      }
      if(request.body.asFormUrlEncoded.get("ccnonempty")(0).toInt == 1){
         carbonCopy = request.body.asFormUrlEncoded.get("copyemail[]").toList.mkString(";")
      }
      if(request.body.asFormUrlEncoded.get("bccnonempty")(0).toInt == 1){
         blindCarbonCopy = request.body.asFormUrlEncoded.get("secretemail[]").toList.mkString(";")
      }

      EmailSet.deleteByUser(user.id)
      EmailSet.insertRow(user.id, send, carbonCopy, blindCarbonCopy)
      if(request.body.asFormUrlEncoded.get("sendnonempty")(0).toInt == 1){
        Ok(Json.toJson(1))
      }
      else{
        Ok(Json.toJson(0))
      }
    }
  }
}
