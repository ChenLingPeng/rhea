package controllers

import play.api.Play
import play.api.mvc.{Action, Controller}
import play.api.Play.current

/**
 * Created by chenlingpeng on 2014/11/11.
 */
object PicApp extends Controller {
  def index(id: Long) = Action {
    implicit request =>
      val file = Play.application.getFile("1.jpg")
      val source = scala.io.Source.fromFile(file)(scala.io.Codec.ISO8859)
      val byteArray = source.map(_.toByte).toArray
      source.close()
      Ok(byteArray).as("image/jpeg")
  }

  def gitlab() = Action {
    implicit request =>
      val body = request.body.asJson.get
      println(body)
      println(body \ "user_name")
      println(body \ "repository" \ "name")
      Ok
  }
}
