package controllers

import be.objectify.deadbolt.scala.DeadboltActions
import models.{User, Project, Task, ProjectMember}
import org.slf4j.LoggerFactory
import play.api.Logger
import play.api.cache.Cache
import play.api.libs.json.Json
import play.api.mvc.{Controller, Action}
import security.{MyDynamicResourceHandler, MyDeadboltHandler}


/**
 * Created with IntelliJ IDEA.
 * User: chenlingpeng
 * Date: 2014/10/23
 * Time: 10:46
 *
 */
object ProjectApp extends Controller with DeadboltActions {

  def create = {
    Restrict(Array("2"), new MyDeadboltHandler(failResult = Ok(views.html.neonlogin(Application.loginForm)))) {
      Action { implicit request =>
        val user = User.getUserById(request.session.get("session.id").get.toLong)
        val name = request.body.asFormUrlEncoded.get("projectname")(0)
        val description = request.body.asFormUrlEncoded.get("projectdesc")(0)
        val groupid = user.groupid.get
        val teamid = user.teamid
        val ownbyid = request.body.asFormUrlEncoded.get("ownby")(0).toLong
        val projectid = Project.createProject(name, description, ownbyid, groupid, teamid)
        if(request.body.asFormUrlEncoded.get("nonempty")(0).toInt == 1) {
          val members = request.body.asFormUrlEncoded.get("othermembers[]").toList
          members.map( memberid => {
            val member = User.getUserById(Integer.parseInt(memberid))
            ProjectMember.insertMember(projectid, member.id, member.role)
          })
        }
        val project = Project.getProjectByIdFromCache(projectid)
        Logger.info(s"${user.email} create project $name")

        Ok(project.jsonObj)
      }
    }
  }

  def delete(id: Long) = {
    Dynamic(String.valueOf(id), "deleteproject", deadboltHandler = new MyDeadboltHandler(failResult = Ok(views.html.neonlogin(Application.loginForm)), dynamicResourceHandler = Some(new MyDynamicResourceHandler))) {
      Action { implicit request =>
        val user = User.getUserById(request.session.get("session.id").get.toLong)
        val result = Project.deleteProject(id)
        result match {
          case 1 =>
            Task.deleteTaskByProject(id)
            Logger.info(s"${user.email} delete project ${id}")
            Ok(Json.toJson(1))
          case _ =>
            Ok(Json.toJson(0))
        }
      }
    }
  }

  def modify(id: Long) = {
    Dynamic(String.valueOf(id), "editproject", deadboltHandler = new MyDeadboltHandler(failResult = Ok(views.html.neonlogin(Application.loginForm)), dynamicResourceHandler = Some(new MyDynamicResourceHandler))) {
      Action { implicit request =>
        val user = User.getUserById(request.session.get("session.id").get.toLong)
        val description = request.body.asFormUrlEncoded.get("projectdesc")(0)
        val ownby = request.body.asFormUrlEncoded.get("ownby")(0).toLong
        val pstate = request.body.asFormUrlEncoded.get("pstate")(0).toLong
        Project.modifyProject(id, description, ownby, pstate)
        ProjectMember.deleteMemberByProjectId(id)
        if(request.body.asFormUrlEncoded.get("nonempty")(0).toInt == 1){
          val members = request.body.asFormUrlEncoded.get("othermem[]").toList
          members.map( memberid => {
            val member = User.getUserById(Integer.parseInt(memberid))
            ProjectMember.insertMember(id, member.id, member.role)
          })
        }

        val project = Project.getProjectByIdFromCache(id)
        Logger.info(s"${user.email} modify project ${project.name}")
        Ok(project.jsonObj)
      }
    }
  }


  def list() = SubjectPresent(new MyDeadboltHandler(failResult = Ok(views.html.neonlogin(Application.loginForm)))) {
    Action { implicit request =>
      val user = User.getUserById(request.session.get("session.id").get.toLong)
      Ok(views.html.index(user))
    }
  }

  def listByTeam() = SubjectPresent(new MyDeadboltHandler(failResult = Ok(views.html.neonlogin(Application.loginForm)))) {
    Action { implicit request =>
      val tid = request.body.asFormUrlEncoded.get("tid")(0).toLong
      val projects = Project.getProjectsByTeamId(tid)

      var plist = Json.arr()
      projects.foreach { project =>
        plist = plist.append(project.jsonObj)
      }
      Ok(plist)
    }
  }

  def index = SubjectPresent(new MyDeadboltHandler(failResult = Ok(views.html.neonlogin(Application.loginForm)))) {
    Action { implicit request =>
      val user = User.getUserById(request.session.get("session.id").get.toLong)
      val groupmembers = User.getAllUserByGroupId(user.groupid.getOrElse(-1))
      Logger.info(s"${user.email} visit createproject")
      val notgroupmembers = User.getAllUserNotInGroupButInTeam(user.groupid.getOrElse(-1), user.teamid)
      Ok(views.html.createproject(user, groupmembers, notgroupmembers, new MyDeadboltHandler()))
    }
  }

}
