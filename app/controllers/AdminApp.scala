package controllers

import be.objectify.deadbolt.scala.DeadboltActions
import controllers.Application._
import models.{ProjectMember, Project, User, Task, EmailSet,Group}
import play.api.Logger
import play.api.mvc.{Action, Controller}
import security.MyDeadboltHandler
import scala.collection.mutable.Map

/**
 * Created by zhaorui on 2014/11/12.
 */
object AdminApp extends Controller with DeadboltActions{

  def index = {
    SubjectPresent(new MyDeadboltHandler(failResult = Ok(views.html.neonlogin(loginForm)))) {
      Action {
        implicit request =>
          val user = User.getUserById(request.session.get("session.id").get.toLong)
          Logger.info(s"Admin ${user.email} visit index")

          val groups = Group.getAll
          val groupmaps = Map[String, scala.collection.immutable.Map[String,Tuple5[List[Project],List[Project],Long,Long,Long]]] ()
          groups.map(group => {
            val groupmembers = User.getAllUserByGroupId(group.id)
            val usermaps = Map[String,Tuple5[List[Project],List[Project],Long,Long,Long]]()
            groupmembers.map(user => {
              val projects1 = Project.getProjectsByUserId(user.groupid.get)
              val projects2 = ProjectMember.getProjectsByUserId(user.id)
              val tuple3 = Task.Calculate(user.id)
              usermaps += (user.name -> Tuple5(projects1,projects2,tuple3._1,tuple3._2,tuple3._3))
            })
            groupmaps += (group.name -> usermaps.toMap)
          }
          )
          Ok(views.html.admin.index(user,groupmaps.toMap))
      }
    }
  }

}
