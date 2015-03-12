package models

import anorm.SqlParser._
import anorm._
import play.api.cache.Cache
import play.api.db.DB
import play.api.Play.current

/**
 * Created with IntelliJ IDEA.
 * User: chenlingpeng
 * Date: 2014/10/22
 * Time: 21:15
 *
 */
case class Team(id: Long, name: String, description: Option[String], ownby: Long)

object Team {

  private val idCacheKey = "cache.team.id."

  val simple = {
    get[Long]("team.id") ~
      get[String]("team.name") ~
      get[Option[String]]("team.description") ~
      get[Long]("team.ownby") map { case id ~ name ~ description ~ ownby =>
      Team(id, name, description, ownby)
    }
  }

  def getAll = {
    DB.withConnection { implicit c =>
      SQL("select * from team").as(simple *)
    }
  }

  def getTeamById(id: Long) = {
    DB.withConnection { implicit c =>
      SQL("select * from team where id={id}").on('id -> id).as(simple.single)
    }
  }

  def getTeamByIdFromCache(id: Long) = {
    Cache.getOrElse[Team](idCacheKey + id) {
      getTeamById(id)
    }
  }

  def getAllTeamByOwnby(ownby: Long) = {
    DB.withConnection { implicit c =>
      SQL("select * from team where ownby = {ownby}").on('ownby -> ownby).as(simple.*)
    }
  }

}