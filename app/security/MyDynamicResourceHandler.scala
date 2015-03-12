package security

import be.objectify.deadbolt.scala.{DeadboltHandler, DynamicResourceHandler}
import models.{Task, Project, User}
import play.api.mvc.{AnyContent, Request}

/**
 * Created with IntelliJ IDEA.
 * User: chenlingpeng
 * Date: 2014/10/24
 * Time: 10:00
 *
 */
class MyDynamicResourceHandler extends DynamicResourceHandler {
  // meta should be 'project' or 'task'
  override def isAllowed[A](name: String, meta: String, deadboltHandler: DeadboltHandler, request: Request[A]): Boolean = {
    val uid = request.session.get("session.id").get.toLong
    val password = request.session.get("session.password").get
    val user = User.getUserByIdFromCache(uid)
    if(password.equals(user.password)){
      if (meta == "deleteproject" || meta == "editproject") {
        if (user.role >= 2) {
          val project = Project.getProjectByIdFromCache(name.toLong)
          if (project.groupid == user.groupid.get) {
            true
          } else {
            false
          }
        } else {
          false
        }
      } else if(meta == "createproject"){
        if(user.role >= 2){
          true
        }else{
          false
        }
      }else if (meta == "task") {
        if (user.role >= 1) {
          val task = Task.getTaskById(name.toLong)
          if (task.ownby == user.id){
            true
          }else{
            false
          }

        } else {
          false
        }
      } else {
        false
      }
    }else{
      false
    }
  }

  override def checkPermission[A](permissionValue: String, deadboltHandler: DeadboltHandler, request: Request[A]): Boolean = {
    false
  }
}
