package models

import anorm.SqlParser._
import anorm._
import play.api.cache.Cache
import play.api.db.DB
import play.api.Play.current
import play.api.libs.json.Json
import util.TimeFormatUtil

/**
 * Created with IntelliJ IDEA.
 * User: chenlingpeng
 * Date: 2014/10/22
 * Time: 21:17
 *
 */


case class Project(id: Long, name: String, description: String, state: Int, ctime: Long, groupid: Long, teamid: Long, ownby: Long) {
  def jsonObj = {
    Json.obj("id" -> id,
             "name" -> name,
             "des" -> description,
             "state" -> {
               state match {
                 case 1 => "正在进行"
                 case 2 => "废弃"
                 case 3 => "已完成"
               }
             },
             "ctime" -> TimeFormatUtil.toReadableFormat(ctime),
             "ctimetitle" -> TimeFormatUtil.toLocalDate(ctime),
             "groupid" -> groupid,
             "group" -> Group.getGroupByIdFromCache(groupid).name,
             "ownbyid" ->ownby,
             "ownby" -> User.getUserByIdFromCache(ownby).name
    )
  }
}

object Project {

  private val idCacheKey = "cache.project.id."

  val simple = {
    get[Long]("project.id") ~
      get[String]("project.name") ~
      get[String]("project.description") ~
      get[Int]("project.state") ~
      get[Long]("project.ctime") ~
      get[Long]("project.groupid") ~
      get[Long]("project.teamid") ~
      get[Long]("project.ownby") map { case id ~ name ~ description ~ state ~ ctime ~ groupid ~ teamid ~ ownby =>
      Project(id, name, description, state, ctime, groupid, teamid, ownby)
    }
  }

  def getAll = {
    DB.withConnection { implicit c =>
      SQL("select * from project").as(simple *)
    }
  }

  def getProjectById(id: Long) = {
    DB.withConnection { implicit c =>
      SQL("select * from project where id = {id}").on('id -> id).as(simple.single)
    }
  }

  def getProjectByName(name: String) = {
    DB.withConnection { implicit c =>
      SQL("select * from project where name = {name}").on('name -> name).as(simple.single)
    }
  }

  def getProjectByIdFromCache(id: Long) = {
    Cache.getOrElse[Project](idCacheKey + id) {
      getProjectById(id)
    }
  }

  def listProjects(teamid: Long, groupid: Long, ownby: Long) = {

    val filterCondition: Seq[NamedParameter] =
      Seq('teamid -> teamid, 'groupid -> groupid, 'ownby -> ownby)

    val selectSQL = "select * from project where state != 2 "
    val teamSQL = "and teamid = {teamid} "
    val groupSQL = "and groupid = {groupid} "
    val ownbySQL = "and ownby = {ownby} "
    val orderbySQL = "order by 5 desc"

    val integratedSQL = selectSQL + {
      if (teamid > 0) teamSQL else ""
    } + {
      if (groupid > 0) groupSQL else ""
    } + {
      if (ownby > 0) ownbySQL else ""
    } +
      orderbySQL

    DB.withConnection { implicit c =>
      SQL(integratedSQL).on(filterCondition: _*).as(simple.*)
    }
  }

  def createProject(name: String, description: String, ownby: Long, groupid: Long, teamid: Long) = {
    DB.withConnection { implicit c =>
      val pid:Option[Long] = SQL(
        """insert into project (name, description, state, ctime, groupid, teamid, ownby)
          values ({name}, {description}, {state}, {ctime}, {groupid}, {teamid}, {ownby})
        """).on(
          'name -> name,
          'description -> description,
          'state -> 1,
          'ctime -> System.currentTimeMillis(),
          'groupid -> groupid,
          'teamid -> teamid,
          'ownby -> ownby
        ).executeInsert()
      pid.get
    }
  }

  def getProjectsByTeamId(teamid: Long) = {
    DB.withConnection { implicit c =>
      SQL("select * from project where teamid={teamid} and state!=2").on('teamid -> teamid).as(simple *)
    }
  }

  def getProjectsByUserId(groupid: Long) = {
    DB.withConnection { implicit c =>
      SQL("select * from project where groupid={groupid} and state!=2").on('groupid -> groupid).as(simple *)
    }
  }

  def getProjectsDoingByUserId(groupid: Long) = {
    DB.withConnection { implicit c =>
      SQL("select * from project where groupid={groupid} and state=1").on('groupid -> groupid).as(simple *)
    }
  }

  def deleteProject(id: Long) = {
    DB.withConnection {
      implicit c =>
        val result = SQL("update project set state =2 where id = {id}")
          .on('id -> id).executeUpdate()

        Cache.remove(idCacheKey+id)
        result
    }
  }

  def modifyProject(id: Long, desc: String, ownbyid: Long, pstate: Long) = {
    DB.withConnection { implicit c =>
      SQL("update project set description = {desc},ctime = {ctime},ownby = {ownby},state = {pstate} where id = {id}")
        .on(
          'desc -> desc,
          'ctime -> System.currentTimeMillis(),
          'ownby -> ownbyid,
          'pstate -> pstate,
          'id -> id).execute()

      val project = getProjectById(id)
      Cache.set(idCacheKey+id, project)
    }
  }

  val simple2 = {
    get[Long]("user.id") ~
      get[String]("project.name")  map { case userid ~ projectname =>
      Tuple2(userid, projectname)
    }
  }

  def getProjectsAlluserByTeam(teamid: Long) = {
    DB.withConnection { implicit c =>
      SQL(
        """
          select user.id,project.name from user,groups,project where user.groupid = groups.id
          and groups.id = project.groupid and user.teamid={teamid}
        """.stripMargin).on('teamid -> teamid).as(simple2.*)
    }
  }
}
