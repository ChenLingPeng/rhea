package security

import be.objectify.deadbolt.core.models.Subject
import be.objectify.deadbolt.scala.{DeadboltHandler, DynamicResourceHandler}
import models.User
import play.api.Logger
import play.api.mvc.{Results, Security, Result, Request}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Created with IntelliJ IDEA.
 * User: chenlingpeng
 * Date: 2014/10/24
 * Time: 09:47
 *
 */
class MyDeadboltHandler(dynamicResourceHandler: Option[DynamicResourceHandler] = None, failResult: Result = Results.BadRequest) extends DeadboltHandler {
  override def beforeAuthCheck[A](request: Request[A]): Option[Future[Result]] = None

  override def getDynamicResourceHandler[A](request: Request[A]): Option[DynamicResourceHandler] = {
    if (dynamicResourceHandler.isDefined) dynamicResourceHandler
    else Some(new MyDynamicResourceHandler())
  }

  override def getSubject[A](request: Request[A]): Future[Option[Subject]] = {
    Future {
      val uid = request.session.get("session.id")
      val password = request.session.get("session.password")
      if (uid.isDefined && password.isDefined ) {
        val user = User.getUserByIdFromCache(uid.get.toLong)
        if(password.get == user.password){
          Some(user)
        }else {
          None
        }
      } else {
        None
      }
    }
  }

  override def onAuthFailure[A](request: Request[A]): Future[Result] = Future(failResult)
}
