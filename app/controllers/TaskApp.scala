package controllers

import be.objectify.deadbolt.scala.DeadboltActions
import models.{ User, Task}
import org.slf4j.LoggerFactory
import play.api.Logger
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.Json
import play.api.mvc.{Action, Controller}
import security.{MyDynamicResourceHandler, MyDeadboltHandler}

/**
 * Created with IntelliJ IDEA.
 * User: chenlingpeng
 * Date: 2014/10/23
 * Time: 10:48
 *
 */
object TaskApp extends Controller with DeadboltActions {

  val taskForm = Form(tuple("projectname" -> nonEmptyText(), "description" -> nonEmptyText()))

  def list() = SubjectPresent(new MyDeadboltHandler) {
    Action { implicit request =>
      val page = request.body.asFormUrlEncoded.get("page")(0).toInt
      val projectid = request.body.asFormUrlEncoded.get("pid")(0).toLong
      val uid = request.body.asFormUrlEncoded.get("uid")(0).toLong
      val groupid = request.body.asFormUrlEncoded.get("gid")(0).toLong
      val state = request.body.asFormUrlEncoded.get("state")(0).toInt
      val offset = request.body.asFormUrlEncoded.get("offset")(0).toLong
      val tasks = Task.filter(page, uid, projectid, groupid, state, offset)
      var tlist = Json.arr()
      tasks.foreach { task =>
        tlist = tlist.append(task.jsonObj)
      }
      Ok(tlist)
    }
  }



  def create = {
    Restrict(Array("1"), new MyDeadboltHandler(failResult = Ok(views.html.neonlogin(Application.loginForm)))) {
      Action { implicit request =>
        val user = User.getUserByIdFromCache(request.session.get("session.id").get.toLong)
        val pid = request.body.asFormUrlEncoded.get("pid")(0).toLong
        val desc = request.body.asFormUrlEncoded.get("desc")(0)
        val stateid=request.body.asFormUrlEncoded.get("stateid")(0).toInt
        val tid = Task.createTask(desc, stateid, user.id, pid, user.groupid.get, user.teamid)
        val task = Task.getTaskById(tid)
        Logger.info(s"${user.email} create task")
        Ok(task.jsonObj)
      }
    }
  }

  def modify(id: Long) = {
    Dynamic(String.valueOf(id), "task", deadboltHandler = new MyDeadboltHandler(failResult = Ok(views.html.neonlogin(Application.loginForm)), dynamicResourceHandler = Some(new MyDynamicResourceHandler))) {
      Action { implicit request =>
        val user = User.getUserByIdFromCache(request.session.get("session.id").get.toLong)
        val stateid = request.body.asFormUrlEncoded.get("editstateid")(0).toInt
        Task.modifyTaskState(id, stateid)
        val task = Task.getTaskById(id)
        Logger.info(s"${user.email} modify task")
        Ok(task.jsonObj)
      }
    }
  }

  def delete(id: Long) = {
    Dynamic(String.valueOf(id), "task", deadboltHandler = new MyDeadboltHandler(failResult = Ok(views.html.neonlogin(Application.loginForm)), dynamicResourceHandler = Some(new MyDynamicResourceHandler))) {
      Action { implicit request =>
        val task = Task.getTaskById(id)
        if(task.createtime.isEmpty || System.currentTimeMillis()-task.createtime.get>10*60*1000){
          Ok(Json.toJson(0))
        } else {
          val user = User.getUserByIdFromCache(request.session.get("session.id").get.toLong)
          val res = Task.deleteTask(id)
          Logger.info(s"${user.email} delete task")
          if (res == 1) {
            Ok(Json.toJson(1))
          } else {
            Ok(Json.toJson(0))
          }
        }
      }
    }
  }
}
