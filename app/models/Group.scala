package models

import anorm.SqlParser._
import anorm._
import play.api.cache.Cache
import play.api.db.DB
import play.api.Play.current
import play.api.libs.json.Json

/**
 * Created with IntelliJ IDEA.
 * User: chenlingpeng
 * Date: 2014/10/22
 * Time: 21:11
 *
 */
case class Group(id: Long, name: String, description: Option[String], teamid: Long, ownby: Long){
  def jsonObj = {
    Json.obj(
      "id" -> id,
      "desc" -> description,
      "ownby" -> User.getUserByIdFromCache(ownby).name,
      "ownbyid" -> ownby
    )
  }
}

object Group {

  private val idCacheKey = "cache.groups.id."

  val simple = {
    get[Long]("groups.id") ~
      get[String]("groups.name") ~
      get[Option[String]]("groups.description") ~
      get[Long]("groups.teamid") ~
      get[Long]("groups.ownby") map { case id ~ name ~ description ~ teamid ~ ownby =>
      Group(id, name, description, teamid, ownby)
    }
  }

  def getAll = {
    DB.withConnection { implicit c =>
      SQL("select * from groups").as(simple *)
    }
  }

  def getGroupById(id: Long) = {
    DB.withConnection { implicit c =>
      SQL("select * from groups where id={id}").on('id -> id).as(simple.single)
    }
  }

  def getGroupByIdFromCache(id: Long) = {
    Cache.getOrElse[Group](idCacheKey + id) {
      getGroupById(id)
    }
  }

  def getAllGroupByTeamId(teamid: Long) = {
    DB.withConnection { implicit c =>
      SQL("select * from groups where teamid = {teamid}").on('teamid -> teamid).as(simple.*)
    }
  }

  def getAllGroupByOwnby(ownby: Long) = {
    DB.withConnection { implicit c =>
      SQL("select * from groups where ownby = {ownby}").on('ownby -> ownby).as(simple.*)
    }
  }

  def initGroup(name: String, description: String, teamid: Long, ownby: Long = -1) = {
    DB.withConnection { implicit c =>
      val groupid:Option[Long]=SQL(
        """insert into groups (name,description,teamid,ownby) values
          ({name},{description},{teamid},{ownby})
        """.stripMargin).on('name -> name,
                             'description -> description,
                             'teamid -> teamid,
                             'ownby -> ownby).executeInsert()

      groupid.get
    }
  }
}
