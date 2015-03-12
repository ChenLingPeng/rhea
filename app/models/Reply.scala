package models

import java.util

import _root_.util.TimeFormatUtil
import anorm.SqlParser._
import anorm._
import play.api.Logger
import play.api.cache.Cache
import play.api.db.DB
import play.api.Play.current
import play.api.libs.json.{JsObject, Json, JsValue}
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
/**
 * User: zhaorui
 * Date: 2014/12/8
 * Time: 14:43
 */
case class Reply(id: Long, task_id: Long, ownby: Long, rtime: Long, description: String) {
  def jsonObj = {
    Json.obj(
      "id" -> id,
      "task_id" -> task_id,
      "ownby" -> User.getUserByIdFromCache(ownby).name,
      "ownbyid" -> User.getUserByIdFromCache(ownby).id,
      "timestamp" -> rtime,
      "rtime" -> TimeFormatUtil.toReadableFormat(rtime),
      "rtimetitle" -> TimeFormatUtil.toLocalDate(rtime),
      "description" -> description)
  }
}

object Reply {
  val simple = {
    get[Long]("reply.id") ~
      get[Long]("reply.task_id") ~
      get[Long]("reply.ownby") ~
      get[Long]("reply.rtime") ~
      get[String]("reply.description") map { case id ~ task_id ~ ownby ~ rtime ~ description =>
      Reply(id, task_id, ownby, rtime, description)
    }
  }

  def create(taskid: Long, ownby: Long, des: String) = {
    DB.withConnection { implicit c =>
      val rid: Option[Long] = SQL(
        """
          insert into reply (task_id,ownby,rtime,description)
           values ({task_id},{ownby},{rtime},{description})
        """.stripMargin)
        .on('task_id -> taskid,
        'ownby -> ownby,
        'rtime -> System.currentTimeMillis(),
        'description -> des).executeInsert()

      rid.get
    }
  }

  def getReplyById(rid: Long) = {
    DB.withConnection { implicit c =>
      SQL(
        """
          select * from reply where id = {rid}
        """.stripMargin)
      .on('rid -> rid).as(simple.single)
    }
  }

  def getReplysByTask(tid: Long) = {
    DB.withConnection { implicit c =>
      SQL(
        """
          select * from reply where task_id = {tid}
        """.stripMargin)
        .on('tid -> tid).as(simple.*)
    }
  }

  def replyNums(tid: Long) = {
    DB.withConnection { implicit c =>
      SQL(
        """
          select count(*) from reply where task_id = {tid}
        """.stripMargin)
        .on('tid -> tid).as(scalar[Long].single)
    }
  }
}