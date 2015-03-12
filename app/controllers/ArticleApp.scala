package controllers

import be.objectify.deadbolt.scala.DeadboltActions
import models.{Article, User}
import play.api.data.Forms._
import play.api.data._
import play.api.libs.json.Json
import play.api.mvc.{Action, Controller}
import security.MyDeadboltHandler
import util.TimeFormatUtil

/**
 * Created by chenlingpeng on 2015/1/13.
 */
object ArticleApp extends Controller with DeadboltActions {
  val mdForm = Form(
  tuple(
  "title"->nonEmptyText(),
  "mdtxt"->nonEmptyText()
  )
  )

  def edit = {
    SubjectPresent(new MyDeadboltHandler(failResult = Ok(views.html.neonlogin(Application.loginForm)))) {
      Action {
        implicit request =>
          val user = User.getUserById(request.session.get("session.id").get.toLong)
          Ok(views.html.edit())
      }
    }
  }

  def view(id: Long) = {
    SubjectPresent(new MyDeadboltHandler(failResult = Ok(views.html.neonlogin(Application.loginForm)))) {
      Action {
        implicit request =>
          val user = User.getUserById(request.session.get("session.id").get.toLong)
          Ok(views.html.article(user, id))
      }
    }
  }

  def view2 = {
    SubjectPresent(new MyDeadboltHandler(failResult = Ok(views.html.neonlogin(Application.loginForm)))) {
      Action {
        implicit request =>
          mdForm.bindFromRequest.fold(
            formWithErrors => {
              println("*"*20)
              Ok(views.html.edit())
            },
            value => {
              val uid = request.session.get("session.id").get.toLong
              val id = Article.createArticle(uid, value._1, value._2)
//              lastSubmit=Some(value)
//              val user = User.getUserById(request.session.get("session.id").get.toLong)
              Redirect("/view/"+id)
//              Ok(views.html.article(user, id))
            }
          )
      }
    }
  }

  def list = {SubjectPresent(new MyDeadboltHandler(failResult = Ok(views.html.neonlogin(Application.loginForm)))) {
    Action {
      implicit request =>
        val user = User.getUserById(request.session.get("session.id").get.toLong)
        val arts = Article.list()
        Ok(views.html.articlelist(user, arts))
    }
  }
  }

  def art(id: Long) = {SubjectPresent(new MyDeadboltHandler(failResult = Ok(views.html.neonlogin(Application.loginForm)))) {
    Action {
      implicit request =>
                val article = Article.getById(id)
                Ok(Json.obj("status"->"success",
                "data"->article.content,
                "title"->article.title,
                "user"->User.getUserByIdFromCache(article.uid).name,
                "ctime"->TimeFormatUtil.toLocalDate(article.ctime)
                ))
    }
  }
  }
}
