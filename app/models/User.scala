package models

import java.util

import anorm.SqlParser._
import anorm._
import be.objectify.deadbolt.core.models.{Role, Permission, Subject}
import org.apache.commons.codec.digest.DigestUtils
import play.api.cache.Cache
import play.api.db.DB
import play.api.Play.current
import play.libs.Scala

/**
 * Created with IntelliJ IDEA.
 * User: chenlingpeng
 * Date: 2014/10/22
 * Time: 20:08
 *
 */
// role 1(team),2(group),3(member)
case class User(id: Long, email: String, name: String, password: String, role: Int, groupid: Option[Long], teamid: Long) extends Subject {
  override def getRoles: util.List[_ <: Role] = {
    Scala.asJava((1 to role).map { case index => SecurityRole(index.toString)
    })
  }

  override def getPermissions: util.List[_ <: Permission] = {
    Scala.asJava((1 to role).map { case index => UserPermission(index.toString)
    })
  }

  override def getIdentifier: String = {
    email
  }
}

case class UserPermission(value: String) extends Permission {
  override def getValue: String = value
}

case class SecurityRole(roleName: String) extends Role {
  def getName: String = roleName
}

object User {

  private val idCacheKey = "cache.user.id."
  private val emailCacheKey = "cache.user.email."

  val simple = {
    get[Long]("user.id") ~
      get[String]("user.email") ~
      get[String]("user.name") ~
      get[String]("user.password") ~
      get[Int]("user.role") ~
      get[Option[Long]]("user.groupid") ~
      get[Long]("user.teamid") map { case id ~ email ~ name ~ password ~ role ~ groupid ~ teamid =>
      User(id, email, name, password, role, groupid, teamid)
    }
  }
  val simple2 = {
    get[Long]("groups.id")~
    get[String]("groups.name")~
    get[Long]("user.id")~
    get[String]("user.name") map { case groupid ~ groupname ~ userid ~ username =>
      Tuple4(groupid,groupname,userid,username)
    }
  }

  def getTuple4ByTeam(teamid: Long) = {
    DB.withConnection { implicit c =>
      SQL(
        """
          select groups.id,groups.name,user.id,user.name from user,groups
          where user.groupid=groups.id and user.teamid={teamid}
        """.stripMargin)
      .on('teamid -> teamid).as(simple2.*)
    }
  }

  def getAll = {
    DB.withConnection { implicit c =>
      SQL("select * from user").as(simple *)
    }
  }

  def getUserById(id: Long) = {
    DB.withConnection { implicit c =>
      SQL("select * from user where id={id}").on('id -> id).as(simple.single)
    }
  }

  def getUserByIdFromCache(id: Long) = {
    Cache.getOrElse[User](idCacheKey + id) {
      getUserById(id)
    }
  }

  def getUserByEmail(email: String) = {
    DB.withConnection { implicit c =>
      SQL("select * from user where email={email}").on('email -> email).as(simple.singleOpt)
    }
  }

  def getUserByEmailFromCache(email: String) = {
    Cache.getOrElse[User](emailCacheKey + email) {
      getUserByEmail(email).get
    }
  }

  def getAllUserByGroupId(groupid: Long) = {
    DB.withConnection { implicit c =>
      SQL("select * from user where groupid = {groupid}").on('groupid -> groupid).as(simple.*)
    }
  }

  def getAllUserNotInGroupButInTeam(groupid: Long, teamid: Long) = {
    DB.withConnection { implicit c =>
      SQL("select * from user where teamid = {teamid} and groupid != {groupid}")
        .on('teamid -> teamid, 'groupid -> groupid).as(simple.*)
    }
  }

  def getAllUserByTeamId(teamid: Long) = {
    DB.withConnection { implicit c =>
      SQL("select * from user where teamid = {teamid}").on('teamid -> teamid).as(simple.*)
    }
  }

  def getOtherUsers(userid: Long) = {
    DB.withConnection { implicit c =>
      SQL("select * from user where id != {userid}").on('userid -> userid).as(simple.*)
    }
  }

  def authenticate(email: String, password: String): Boolean = {
    val user = getUserByEmail(email)
    if (user.isDefined) {
      user.get.password == password
    } else {
      false
    }
  }

  def resetPsw(old:String, email: String, password: String) = {
    DB.withConnection { implicit c =>
      val res = SQL("update user set password = {password} where email = {email} and password = {old}")
        .on('password -> password, 'email -> email, 'old -> old).executeUpdate()
      if(res>0){
        val user = User.getUserByEmail(email)
        Cache.set(emailCacheKey+email,user)
        Cache.set(idCacheKey+user.get.id,user)
      }
      res>0
    }
  }

  def forgetPsw(email: String, password: String) = {
    DB.withConnection { implicit c =>
      val res = SQL("update user set password = {password} where email = {email}")
        .on('password -> password, 'email -> email).executeUpdate()
      if(res>0){
        val user = User.getUserByEmail(email)
        Cache.set(emailCacheKey+email,user)
        Cache.set(idCacheKey+user.get.id,user)
      }
      res>0
    }
  }




  def main(args: Array[String]) {
    val s = DigestUtils.md5Hex(DigestUtils.md5Hex("111111")+"zhaorui@neotel.com.cn")
    println(s)
  }
}
