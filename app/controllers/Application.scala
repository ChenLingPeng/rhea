package controllers

import be.objectify.deadbolt.scala.DeadboltActions
import models._
import org.apache.commons.codec.digest.DigestUtils
import org.slf4j.LoggerFactory
import play.api._
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.Json
import play.api.mvc._
import security.{MyDeadboltHandler}
import util._
import scala.collection.mutable

object Application extends Controller with DeadboltActions{
  val loginForm = Form(
    tuple(
      "email" -> email,
      "password" -> nonEmptyText()
    )
  )

  val registerForm = Form(
    tuple(
      "username" -> nonEmptyText(),
      "email" -> email,
      "password" -> nonEmptyText(),
      "repassword" -> nonEmptyText(),
      "role" -> number(),
      "group" -> longNumber(),
      "team" -> longNumber()
    )
  )

  def index = {
    SubjectPresent(new MyDeadboltHandler(failResult = Ok(views.html.neonlogin(loginForm)))) {
      Action {
        implicit request =>
          val user = User.getUserById(request.session.get("session.id").get.toLong)
          Logger.info(s"${user.email} visit index")
          Ok(views.html.index(user))
      }
    }
  }

  def projectIndex(projectid: Long) = {
    SubjectPresent(new MyDeadboltHandler(failResult = Ok(views.html.neonlogin(loginForm)))) {
      Action {
        implicit request =>
          val user = User.getUserById(request.session.get("session.id").get.toLong)
          val project = Project.getProjectByIdFromCache(projectid)
          val groupmembers = User.getAllUserByGroupId(user.groupid.getOrElse(-1))
          val projectothers = ProjectMember.getMembersByProjectId(projectid)
          val allothers = User.getAllUserNotInGroupButInTeam(user.groupid.getOrElse(-1), user.teamid)
          Ok(views.html.showproject(user, groupmembers, projectothers, allothers, projectid, project.name, project.description, User.getUserById(project.ownby).name, new MyDeadboltHandler()))
      }
    }
  }

  def personal = {
    SubjectPresent(new MyDeadboltHandler(failResult = Ok(views.html.neonlogin(loginForm)))) {
      Action {
        implicit request =>
          val user = User.getUserById(request.session.get("session.id").get.toLong)
          val projects = Project.getProjectsDoingByUserId(user.groupid.getOrElse(-1))
          val otherprojects = ProjectMember.getProjectsDoingByUserId(user.id)
          Logger.info(s"${user.email} visit personal")
          Ok(views.html.personal(user, projects, otherprojects))
      }
    }
  }

  def personalforext = {
    SubjectPresent(new MyDeadboltHandler(failResult = Ok(views.html.neonlogin(loginForm)))) {
      Action {
        implicit request =>
          val user = User.getUserById(request.session.get("session.id").get.toLong)
          val projects = Project.getProjectsByUserId(user.groupid.getOrElse(-1))
          val otherprojects = ProjectMember.getProjectsByUserId(user.id)
          Logger.info(s"${user.email} visit personal")
          Ok(views.html.personalforext(user, projects, otherprojects))
      }
    }
  }

//  def calculate = {
//    SubjectPresent(new MyDeadboltHandler(failResult = Ok(views.html.neonlogin(loginForm)))) {
//      Action {
//        implicit request =>
//          val user = User.getUserById(request.session.get("session.id").get.toLong)
//
//          val groups = Group.getAllGroupByTeamId(user.teamid)
//          val groupmaps = Map[String, scala.collection.immutable.Map[String,Tuple5[List[Project],List[Project],Long,Long,Long]]] ()
//          groups.map(group => {
//            val groupmembers = User.getAllUserByGroupId(group.id)
//            val usermaps = Map[String,Tuple5[List[Project],List[Project],Long,Long,Long]]()
//            groupmembers.map(user => {
//              val projects1 = Project.getProjectsByUserId(user.groupid.get)
//              val projects2 = ProjectMember.getProjectsByUserId(user.id)
//              val tuple3 = Task.Calculate(user.id)
//              usermaps += (user.name -> Tuple5(projects1,projects2,tuple3._1,tuple3._2,tuple3._3))
//            })
//            groupmaps += (group.name -> usermaps.toMap)
//          }
//          )
//
//          Ok(views.html.calculate(user,groupmaps.toMap))
//      }
//    }
//  }

  def calculate = {
    SubjectPresent(new MyDeadboltHandler(failResult = Ok(views.html.neonlogin(loginForm)))) {
      Action {
        implicit request =>
          val user = User.getUserById(request.session.get("session.id").get.toLong)

          val groupid_groupname_userid_username = User.getTuple4ByTeam(user.teamid)
          val userid_projectname = Project.getProjectsAlluserByTeam(user.teamid)
          val userid_otherpname = ProjectMember.getProjectAllUsersByTeam(user.teamid)
          val userid_taskstate = Task.getTaskTuple2ThisWeekByTeam(TimeFormatUtil.firstDayOfWeek,user.teamid)

          val usermaps = mutable.HashMap[Long,Tuple5[List[String],List[String],Long,Long,Long]]()

          userid_projectname.map{tuple =>
            if(usermaps.contains(tuple._1)){
              val tuple5 = usermaps.get(tuple._1).get
              val newlist = tuple5._1 ::: List(tuple._2)
              val newTuple5 = Tuple5(newlist,tuple5._2,tuple5._3,tuple5._4,tuple5._5)
              usermaps.put(tuple._1, newTuple5)
            }else{
              val newTuple5 = Tuple5(List(tuple._2),List[String](),0L,0L,0L)
              usermaps.put(tuple._1, newTuple5)
            }
          }

          userid_otherpname.map{tuple =>
            if(usermaps.contains(tuple._1)){
              val tuple5 = usermaps.get(tuple._1).get
              val newlist = tuple5._2 ::: List(tuple._2)
              val newTuple5 = Tuple5(tuple5._1,newlist,tuple5._3,tuple5._4,tuple5._5)
              usermaps.put(tuple._1, newTuple5)
            }else{
              val newTuple5 = Tuple5(List[String](),List(tuple._2),0L,0L,0L)
              usermaps.put(tuple._1, newTuple5)
            }
          }

          userid_taskstate.map{tuple =>
            if(tuple._2 == 3){
              if(usermaps.contains(tuple._1)){
                val tuple5 = usermaps.get(tuple._1).get
                val newTuple5 = Tuple5(tuple5._1,tuple5._2,tuple5._3+1,tuple5._4,tuple5._5)
                usermaps.put(tuple._1, newTuple5)
              }else{
                val newTuple5 = Tuple5(List[String](),List[String](),1L,0L,0L)
                usermaps.put(tuple._1, newTuple5)
              }
            }else if(tuple._2 == 2){
              if(usermaps.contains(tuple._1)){
                val tuple5 = usermaps.get(tuple._1).get
                val newTuple5 = Tuple5(tuple5._1,tuple5._2,tuple5._3,tuple5._4+1,tuple5._5)
                usermaps.put(tuple._1, newTuple5)
              }else{
                val newTuple5 = Tuple5(List[String](),List[String](),0L,1L,0L)
                usermaps.put(tuple._1, newTuple5)
              }
            }else if(tuple._2 == 1){
              if(usermaps.contains(tuple._1)){
                val tuple5 = usermaps.get(tuple._1).get
                val newTuple5 = Tuple5(tuple5._1,tuple5._2,tuple5._3,tuple5._4,tuple5._5+1)
                usermaps.put(tuple._1, newTuple5)
              }else{
                val newTuple5 = Tuple5(List[String](),List[String](),0L,0L,1L)
                usermaps.put(tuple._1, newTuple5)
              }
            }
          }

          val groupmaps = mutable.Map[String, Map[String,Tuple5[List[String],List[String],Long,Long,Long]]] ()
          groupid_groupname_userid_username.map{ tuple =>
            if(groupmaps.contains(tuple._2)){
              val originmaps = groupmaps.get(tuple._2).get
              if(usermaps.contains(tuple._3)){
                val tuple5 = usermaps.get(tuple._3).get
                val newmaps = originmaps + (tuple._4 -> tuple5)
                groupmaps.put(tuple._2,newmaps)
              }
            }else{
              if(usermaps.contains(tuple._3)){
                val tuple5 = usermaps.get(tuple._3).get
                val newmaps = Map[String,Tuple5[List[String],List[String],Long,Long,Long]](tuple._4 -> tuple5)
                groupmaps.put(tuple._2,newmaps)
              }
            }
          }
          Ok(views.html.calculate(user,groupmaps.toMap))
      }
    }
  }

  def others(userid: Long) = {
    SubjectPresent(new MyDeadboltHandler(failResult = Ok(views.html.neonlogin(loginForm)))) {
      Action {
        implicit request =>
          val user = User.getUserById(userid)
          val login = User.getUserById(request.session.get("session.id").get.toLong)
          Logger.info(s"${user.email} visit others")
          Ok(views.html.others(user, login))
      }
    }
  }

  def login = SubjectPresent(new MyDeadboltHandler(failResult = Ok(views.html.neonlogin(loginForm)))){
    Action {
      implicit request =>
        val user = User.getUserById(request.session.get("session.id").get.toLong)
        Logger.info(s"${user.email} autologin")
        Ok(views.html.index(user))
    }
  }

  def logout = Action {
    implicit request =>
      Ok(views.html.neonlogin(loginForm)).withNewSession
  }

  def msg(msg:String) = Action {
    implicit request =>
      val user = User.getUserById(request.session.get("session.id").get.toLong)
      Ok(views.html.index(user))
  }

  def authenticate = Action {
      implicit request =>
        loginForm.bindFromRequest.fold(
          formWithErrors => BadRequest(views.html.neonlogin(formWithErrors)),
          userInfo => {
            val user = User.getUserByEmail(userInfo._1)
            if (user.isDefined) {
              val md5Hex = DigestUtils.md5Hex(userInfo._2+user.get.email)
              if (user.get.password == md5Hex) {
                Logger.info(s"${user.get.email} login OK")
                if(user.get.role == 4){
                  Redirect(routes.AdminApp.index()).withSession("session.id"->user.get.id.toString,"session.password"->user.get.password)
                }else{
                  Redirect(routes.Application.index()).withSession("session.id"->user.get.id.toString,"session.password"->user.get.password)
                }
              } else {
                Logger.info(s"${user.get.email} login fail")
                Ok(views.html.neonlogin(loginForm.fill((userInfo._1, "")).withError("perror", "password error")))
              }
            } else {
              Ok(views.html.neonlogin(loginForm.fill((userInfo._1, "")).withError("merror", "email invalid")))
            }
          }
        )
  }



  def javascriptRoutes = Action { implicit request =>
    Ok(
      Routes.javascriptRouter("jsRoutes")(
      )
    ).as("text/javascript")
  }
}
