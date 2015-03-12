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
 * Time: 21:26
 *
 */
case class ProjectMember(projectid: Long, userid: Long, role: Int)

object ProjectMember {
  val simple = {
    get[Long]("projectmember.projectid") ~
      get[Long]("projectmember.userid") ~
      get[Int]("projectmember.role") map { case projectid ~ userid ~ role =>
      ProjectMember(projectid, userid, role)
    }
  }

  def getMembersByProjectId(projectid: Long) = {
    DB.withConnection { implicit c =>
      SQL(
        """
         select user.id, user.email, user.name, user.password, user.role, user.groupid, user.teamid
         from projectmember, user where projectmember.userid = user.id
         and projectid = {projectid}
        """).on('projectid -> projectid).as(User.simple.*)
    }
  }

  def getProjectsByUserId(userid: Long) = {
    DB.withConnection { implicit c =>
      SQL(
        """
         select project.id, project.name, project.description, project.state, project.ctime, project.groupid, project.teamid, project.ownby
         from projectmember, project where project.state!=2 and projectmember.projectid = project.id
         and projectmember.userid = {userid}
        """).on('userid -> userid).as(Project.simple.*)
    }
  }
  def getProjectsDoingByUserId(userid: Long) = {
    DB.withConnection { implicit c =>
      SQL(
        """
         select project.id, project.name, project.description, project.state, project.ctime, project.groupid, project.teamid, project.ownby
         from projectmember, project where project.state=1 and projectmember.projectid = project.id
         and projectmember.userid = {userid}
        """).on('userid -> userid).as(Project.simple.*)
    }
  }

  def insertMember(projectid: Long, userid: Long, role: Int) = {
    DB.withConnection { implicit c =>
      SQL(
        """
         insert into projectmember values({projectid},{userid},{role})
        """).on('projectid -> projectid,
                 'userid -> userid,
                 'role -> role).executeUpdate()
    }
  }

  def deleteMemberByProjectId(projectid: Long) = {
    DB.withConnection { implicit c =>
      SQL(
        """
         delete from projectmember where projectid = {projectid}
        """).on('projectid -> projectid).executeUpdate()
    }
  }

  val simple2 = {
    get[Long]("projectmember.userid") ~
      get[String]("project.name")  map { case userid ~ projectname =>
      Tuple2(userid,projectname)
    }
  }

  def getProjectAllUsersByTeam(teamid: Long) = {
    DB.withConnection { implicit c =>
      SQL(
        """
          select projectmember.userid,project.name from project,projectmember where projectmember.projectid = project.id
          and project.teamid = {teamid}
        """.stripMargin).on('teamid -> teamid).as(simple2.*)
    }
  }
}
