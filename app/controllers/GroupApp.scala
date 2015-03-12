package controllers

import models.{User, Group}
import play.api.mvc.{Action, Controller}
import be.objectify.deadbolt.scala.DeadboltActions
import security.{MyDynamicResourceHandler, MyDeadboltHandler}
import play.api.libs.json.Json
/**
 * Created by zhaorui on 2014/11/10.
 */
object GroupApp extends Controller with DeadboltActions{

  def create = {
    Restrict(Array("3"), new MyDeadboltHandler(failResult = Ok(views.html.neonlogin(Application.loginForm)))){
      Action{ implicit request =>
        val user = User.getUserById(request.session.get("session.id").get.toLong)
        val gname = request.body.asFormUrlEncoded.get("gname")(0)
        val description = request.body.asFormUrlEncoded.get("desc")(0)
        val gid = Group.initGroup(gname, description, user.teamid)
        val group = Group.getGroupByIdFromCache(gid)
        Ok(group.jsonObj)
      }
    }
  }

}
