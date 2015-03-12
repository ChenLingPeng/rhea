package controllers

import be.objectify.deadbolt.scala.DeadboltActions
import models.{ User, Task, Reply}
import org.slf4j.LoggerFactory
import play.api.Logger
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.Json
import play.api.mvc.{Action, Controller}
import security.{MyDynamicResourceHandler, MyDeadboltHandler}
/**
 * User: zhaorui
 * Date: 2014/12/8
 * Time: 18:48
 */
object ReplyApp extends Controller with DeadboltActions{
  def create(taskid: String) = {
    Restrict(Array("1"), new MyDeadboltHandler(failResult = Ok(views.html.neonlogin(Application.loginForm)))) {
      Action { implicit request =>
        val user = User.getUserByIdFromCache(request.session.get("session.id").get.toLong)
        val tid = taskid.toLong
        val desc = request.body.asFormUrlEncoded.get("reply")(0)

        val rid = Reply.create(tid, user.id, desc)
        val reply = Reply.getReplyById(rid)
        Logger.info(s"${user.email} reply task")
        Ok(reply.jsonObj)
      }
    }
  }

  def listByTask(taskid: String) = SubjectPresent(new MyDeadboltHandler) {
    Action { implicit request =>
      val tid = taskid.toLong
      val replys = Reply.getReplysByTask(tid)
      var rlist = Json.arr()
      replys.foreach { reply =>
        rlist = rlist.append(reply.jsonObj)
      }
      Ok(rlist)
    }
  }

}
