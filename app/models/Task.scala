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
 * Created with IntelliJ IDEA.
 * User: chenlingpeng
 * Date: 2014/10/22
 * Time: 21:21
 *
 */
case class Task(id: Long, description: String, state: Int, ownby: Long, ctime: Long,createtime:Option[Long], projectid: Long, groupid: Long, teamid: Long, pstate: Int) {
  def jsonObj = {
    Json.obj(
      "id" -> id,
      "desc" -> description,
      "stateid" ->state,
      "state" -> {
        state match {
          case 1 => " 即将开始"
          case 2 => " 正在进行"
          case 3 => " 已完成"
          case 4 => " 放弃"
        }
      },
      "ownby" -> User.getUserByIdFromCache(ownby).name,
      "ownbyid" -> User.getUserByIdFromCache(ownby).id,
      "timestamp" -> ctime,
      "ctime" -> TimeFormatUtil.toReadableFormat(ctime),
      "ctimetitle" -> TimeFormatUtil.toLocalDate(ctime),

      "projectname" -> Project.getProjectByIdFromCache(projectid).name,
      "projectid" -> Project.getProjectByIdFromCache(projectid).id,
      "replynum" -> Reply.replyNums(id))
  }
}

object Task {
  val simple = {
    get[Long]("task.id") ~
      get[String]("task.description") ~
      get[Int]("task.state") ~
      get[Long]("task.ownby") ~
      get[Long]("task.ctime") ~
      get[Option[Long]]("task.createtime") ~
      get[Long]("task.projectid") ~
      get[Long]("task.groupid") ~
      get[Long]("task.teamid")~
      get[Int]("task.pstate") map { case id ~ description ~ state ~ ownby ~ ctime ~createtime ~ projectid ~ groupid ~ teamid ~pstate =>
      Task(id, description, state, ownby, ctime,createtime, projectid, groupid, teamid, pstate)
    }
  }

  def getDoneThisWeek(ownby: Long, time: Long, projectid: Long) = {
    DB.withConnection { implicit c =>
      SQL("select * from task where pstate = 1 and projectid = {projectid} and ctime between {start} and {end} and ownby = {ownby} and state = 3")
        .on('projectid -> projectid,
            'start -> time,
            'end -> System.currentTimeMillis(),
            'ownby -> ownby).as(simple *)
    }
  }

  def getDoingThisWeek(ownby: Long, time: Long, projectid: Long) = {
    DB.withConnection { implicit c =>
      SQL("select * from task where pstate = 1 and projectid = {projectid} and ctime between {start} and {end} and ownby = {ownby} and state = 2")
        .on('projectid -> projectid,
          'start -> time,
          'end -> System.currentTimeMillis(),
          'ownby -> ownby).as(simple *)
    }
  }

  def getTodoThisWeek(ownby: Long, time: Long, projectid: Long) = {
    DB.withConnection { implicit c =>
      SQL("select * from task where pstate = 1 and projectid = {projectid} and ctime between {start} and {end} and ownby = {ownby} and state = 1")
        .on('projectid -> projectid,
          'start -> time,
          'end -> System.currentTimeMillis(),
          'ownby -> ownby).as(simple *)
    }
  }

  def getTodo(ownby: Long,  projectid: Long) = {
    DB.withConnection { implicit c =>
      SQL("select * from task where pstate = 1 and projectid = {projectid} and ownby = {ownby} and state = 1")
        .on('projectid -> projectid, 'ownby -> ownby).as(simple *)
    }
  }

  def getDoing(ownby: Long,  projectid: Long) = {
    DB.withConnection { implicit c =>
      SQL("select * from task where pstate = 1 and projectid = {projectid} and ownby = {ownby} and state = 2")
        .on('projectid -> projectid, 'ownby -> ownby).as(simple *)
    }
  }

  def weeklyReport(user:User) = {
    val projects = Project.getProjectsByUserId(user.groupid.getOrElse(-1))
    val others = ProjectMember.getProjectsByUserId(user.id)

    val timestamp = TimeFormatUtil.firstDayOfWeek

    val donethisweekMap = Map[String, List[Task]] ()
    projects.map(project => donethisweekMap += (project.name -> Task.getDoneThisWeek(user.id, timestamp, project.id)))
    others.map(project => donethisweekMap += (project.name -> Task.getDoneThisWeek(user.id, timestamp, project.id)))

    val doingthisweekMap = Map[String, List[Task]] ()
    projects.map(project => doingthisweekMap += (project.name -> Task.getDoingThisWeek(user.id, timestamp, project.id)))
    others.map(project => doingthisweekMap += (project.name -> Task.getDoingThisWeek(user.id, timestamp, project.id)))

    val todoMap = Map[String, List[Task]] ()
    projects.map(project => todoMap += (project.name -> Task.getTodo(user.id, project.id)))
    others.map(project => todoMap += (project.name -> Task.getTodo(user.id, project.id)))

    val doingMap = Map[String, List[Task]] ()
    projects.map(project => doingMap += (project.name -> Task.getDoing(user.id, project.id)))
    others.map(project => doingMap += (project.name -> Task.getDoing(user.id, project.id)))

    Tuple4(donethisweekMap, doingthisweekMap, todoMap, doingMap)
  }

  def getTaskNumThisWeekAllProjectsByUserByState(ownby: Long, time: Long, state: Int) = {
    DB.withConnection { implicit c =>
      SQL("select count(*) from task where ctime between {start} and {end} and ownby = {ownby} and state = {state}")
        .on('start -> time,
          'end -> System.currentTimeMillis(),
          'ownby -> ownby,
          'state -> state).as(scalar[Long].single)
    }
  }

  val simple2 = {
    get[Long]("task.ownby") ~
      get[Int]("task.state") map { case ownby ~ state =>
      Tuple2(ownby, state)
    }
  }

  def getTaskTuple2ThisWeekByTeam(time: Long,teamid: Long) = {
    DB.withConnection { implicit c =>
      SQL(
        """
          select ownby,state from task where ctime between {start} and {end} and teamid= {teamid}
        """.stripMargin)
        .on('start -> time,
          'end -> System.currentTimeMillis(),
          'teamid -> teamid).as(simple2.*)
    }
  }
//  def Calculate(user:User, projects: List[Project], others: List[Project]) = {
//    val timestamp = TimeFormatUtil.firstDayOfWeek
//
//    var donethisweek = 0
//    projects.map(project => donethisweek += Task.getDoneThisWeek(user.id, timestamp, project.id).size)
//    others.map(project => donethisweek += Task.getDoneThisWeek(user.id, timestamp, project.id).size)
//
//    var doingthisweek = 0
//    projects.map(project => doingthisweek += Task.getDoingThisWeek(user.id, timestamp, project.id).size)
//    others.map(project => doingthisweek += Task.getDoingThisWeek(user.id, timestamp, project.id).size)
//
//    var dodothisweek = 0
//    projects.map(project => dodothisweek += Task.getTodoThisWeek(user.id, timestamp, project.id).size)
//    others.map(project => dodothisweek += Task.getTodoThisWeek(user.id, timestamp, project.id).size)
//
//    Tuple3(donethisweek, doingthisweek, dodothisweek)
//  }
  def Calculate(userid: Long) = {
    val timestamp = TimeFormatUtil.firstDayOfWeek

    val donethisweek = Task.getTaskNumThisWeekAllProjectsByUserByState(userid,timestamp,3)
    val doingthisweek = Task.getTaskNumThisWeekAllProjectsByUserByState(userid,timestamp,2)
    val dodothisweek = Task.getTaskNumThisWeekAllProjectsByUserByState(userid,timestamp,1)

    Tuple3(donethisweek, doingthisweek, dodothisweek)
  }

  def getTaskById(id: Long) = {
    DB.withConnection { implicit c =>
      SQL("select * from task where id={id}").on('id -> id).as(simple.single)
    }
  }

  def createTask(description: String, state: Int, ownby: Long, projectid: Long, groupid: Long, teamid: Long) = {
    DB.withConnection { implicit c => {
        val tid:Option[Long] = SQL(
          """insert into task (description, state, ownby, ctime,createtime, projectid, groupid, teamid, pstate)
            values ({description}, {state}, {ownby}, {ctime},{createtime},{projectid}, {groupid}, {teamid}, 1)""".stripMargin)
          .on(
            'description -> description,
            'state -> state,
            'ownby -> ownby,
            'ctime -> System.currentTimeMillis(),
            'createtime -> System.currentTimeMillis(),
            'projectid -> projectid,
            'groupid -> groupid,
            'teamid -> teamid
          ).executeInsert()
        tid.get
      }
    }
  }

  def modifyTaskState(taskid: Long, currentState: Int) = {
    DB.withConnection { implicit c =>
      SQL("update task set state = {currentState},ctime = {ctime} where id = {taskid}")
        .on(
          'currentState -> currentState,
          'ctime -> System.currentTimeMillis(),
          'taskid -> taskid).execute()
    }
  }

  def deleteTaskByProject(projectid: Long) = {
    DB.withConnection {
      implicit c => SQL("update task set pstate = 2 where projectid = {projectid}")
        .on('projectid -> projectid).executeUpdate()
    }
  }

  // page from 1
  def filter(page: Int, uid: Long, projectid: Long, groupid: Long, state: Int, offset: Long) = {
    val sql = new StringBuilder("select * from task ")
    val params = new ArrayBuffer[String]
      if(offset > 0){
        params += "ctime < {offset}"
      }
      if (uid > 0) {
        params += "ownby={ownby}"
      }
      if (projectid > 0) {
        params += "projectid={projectid}"
      }
      if (groupid > 0) {
        params += "groupid={groupid}"
      }
      if (state > 0) {
        params += "state={state}"
      }
    sql.append("where pstate = 1 ")
    if(params.size>0){
      sql.append("and ")
      sql.append(params.toArray.mkString(" and "))
    }
    sql.append(" order by 5 desc limit {pagesize}")
    DB.withConnection { implicit c =>
      SQL(String.valueOf(sql)).
        on(
          'ownby -> uid,
          'projectid -> projectid,
          'groupid -> groupid,
          'state -> state,
          'pagesize -> 15,
          'offset -> offset
        ).as(simple*)
    }
  }


  def deleteTask(id: Long) = {
    DB.withConnection {
      implicit c => SQL("delete from task where id={id}")
        .on('id -> id).executeUpdate()
    }
  }


}
