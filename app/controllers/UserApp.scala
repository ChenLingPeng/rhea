package controllers

import java.io.File

import be.objectify.deadbolt.scala.DeadboltActions
import controllers.Application._
import models.{ProjectMember, Project, User, Task, EmailSet, Group}
import org.apache.commons.codec.digest.DigestUtils
import play.api.{Play, Logger}
import play.api.cache.Cache
import play.api.libs.json.Json
import play.api.mvc.{Action, Controller}
import security.MyDeadboltHandler
import util.{TimeFormatUtil, CodeSender, CodeGenerator}
import play.api.Play.current
import scala.concurrent.duration._


/**
 * Created by chenlingpeng on 2014/11/4.
 */
object UserApp extends Controller with DeadboltActions{

  def sendemail = Action{
    implicit request =>
      Ok(views.html.sendemail())
  }

  def forgetPsw = Action {
    implicit request =>
      val yourEmail = request.body.asFormUrlEncoded.get("yourEmail")(0)
      val user = User.getUserByEmail(yourEmail)
      if(user.nonEmpty){
        val code = CodeGenerator.newCode.toString
        Cache.set(yourEmail, DigestUtils.md5Hex(DigestUtils.md5Hex(code)+yourEmail), Duration(60,MINUTES))
        CodeSender.sendCode(DigestUtils.md5Hex(code), user.get)
        Ok(Json.toJson(1))
      }else{
        Ok(Json.toJson(0))
      }
  }

  def validate(userid: Long, code: String) = Action {
    implicit request =>
      val user = User.getUserById(userid)
      val cachecode = Cache.getAs[String](user.email)
      if(cachecode.nonEmpty){
        if(cachecode.get == DigestUtils.md5Hex(code+user.email)){
          Logger.info(s"${user.email} reset password OK")
          User.forgetPsw(user.email, DigestUtils.md5Hex(DigestUtils.md5Hex("111111")+user.email))
          Ok("密码重置成功！")
        }
        else{
          Ok("密码重置失败！")
        }
      }else{
        Ok("密码重置失败！")
      }

  }


  def editpassword = {
    SubjectPresent(new MyDeadboltHandler(failResult = Ok(views.html.neonlogin(loginForm)))) {
      Action {
        implicit request =>
          val user = User.getUserById(request.session.get("session.id").get.toLong)
          val projects = Project.getProjectsByUserId(user.groupid.getOrElse(-1))
          Ok(views.html.editpassword(user))
      }
    }
  }

  def workpage = {
    SubjectPresent(new MyDeadboltHandler(failResult = Ok(views.html.neonlogin(loginForm)))) {
      Action {
        implicit request =>
          val user = User.getUserById(request.session.get("session.id").get.toLong)
          Logger.info(s"${user.email} visit workpage")

          val report = Task.weeklyReport(user)
          val userResult = EmailSet.getUsersFromSet(user)

          Ok(views.html.workpage(user,report._1.toMap,report._2.toMap,report._3.toMap,report._4.toMap, userResult._1, userResult._2, userResult._3))
      }
    }
  }
  def setemail = {
    SubjectPresent(new MyDeadboltHandler(failResult = Ok(views.html.neonlogin(loginForm)))) {
      Action {
        implicit request =>
          val user = User.getUserById(request.session.get("session.id").get.toLong)
          val users = User.getAll
          val userResult = EmailSet.getUsersFromSet(user)
          Ok(views.html.setemail(user, users, userResult._1.map( user => user.id).toSet[Long], userResult._2.map( user => user.id).toSet[Long], userResult._3.map( user => user.id).toSet[Long]))
      }
    }
  }
  def sendReport = {
    SubjectPresent(new MyDeadboltHandler(failResult = Ok(views.html.neonlogin(loginForm)))) {
      Action(parse.multipartFormData) {
        implicit request =>
          val user = User.getUserById(request.session.get("session.id").get.toLong)
          val otherjob = request.body.asFormUrlEncoded.get("otherdesc").get(0)
          var helpcontent =request.body.asFormUrlEncoded.get("helpcontent").get(0)
          request.body.file("file").map { file =>
            val filename = file.filename
            val contentType = file.contentType

            CodeSender.sendReport(user,otherjob,helpcontent,file.ref.file,filename,contentType.get)
            Ok(views.html.sendsuccess(user))
          }.getOrElse {
            CodeSender.sendReport(user,otherjob,helpcontent)
            Ok(views.html.sendsuccess(user))
          }
      }
    }
  }

  def oldpassword = {
    SubjectPresent(new MyDeadboltHandler(failResult = Ok(views.html.neonlogin(loginForm)))) {
      Action {
        implicit request =>
          val user = User.getUserById(request.session.get("session.id").get.toLong)
          val psw = request.body.asFormUrlEncoded.get("oldpassword")(0)
          val md5Hex = DigestUtils.md5Hex(psw+user.email)
          if(user.password == md5Hex){
            Ok(Json.toJson(1))
          }else{
            Ok(Json.toJson(0))
          }
      }
    }
  }

  def changepsw = SubjectPresent(new MyDeadboltHandler(failResult = Ok(views.html.neonlogin(loginForm)))) {
    Action {
      implicit request =>
        val user = User.getUserById(request.session.get("session.id").get.toLong)
        val oldpsw = request.body.asFormUrlEncoded.get("oldpassword")(0)
        val psw = request.body.asFormUrlEncoded.get("password")(0)
        val oldmd5Hex = DigestUtils.md5Hex(oldpsw+user.email)
        val md5Hex = DigestUtils.md5Hex(psw+user.email)
        val res = User.resetPsw(oldmd5Hex, user.email, md5Hex)
        Logger.info(s"${user.email} change password")
        if(res){
          Ok(Json.toJson(1))
        }else{
          Ok(Json.toJson(0))
        }

    }
  }

  def listUsers = SubjectPresent(new MyDeadboltHandler(failResult = Ok(views.html.neonlogin(loginForm)))) {
    Action {
      implicit request =>
        val user = User.getUserById(request.session.get("session.id").get.toLong)
        val groups = Group.getAllGroupByTeamId(user.teamid)

        val groupUsers: scala.collection.mutable.HashMap[(Long, String), List[User]] = new scala.collection.mutable.HashMap[(Long, String), List[User]]()
        groups.foreach(group =>
          groupUsers.put((group.id, group.name), User.getAllUserByGroupId(group.id))
        )

        Ok(views.html.userlist(user, groupUsers.toMap))


    }
  }
}
