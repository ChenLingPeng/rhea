package models

import anorm.SqlParser._
import anorm._
import play.api.cache.Cache
import play.api.db.DB
import play.api.Play.current

/**
 * Created by zhaorui on 2014/11/10.
 */
case class EmailSet(userid: Long, send: String, carbonCopy: String, blindCarbonCopy: String) {

}

object EmailSet {
  private val idCacheKey = "cache.emailSet.id."

  val simple = {
    get[Long]("emailSet.userid") ~
      get[String]("emailSet.send") ~
      get[String]("emailSet.carbonCopy") ~
      get[String]("emailSet.blindCarbonCopy") map { case userid ~ send ~ carbonCopy ~ blindCarbonCopy =>
      EmailSet(userid, send, carbonCopy, blindCarbonCopy)
    }
  }

  def insertRow(userid: Long, send: String, carbonCopy: String, blindCarbonCopy: String) = {
    DB.withConnection {
      implicit c =>
        SQL(
          """
           insert into emailSet values ({userid},{send},{carbonCopy},{blindCarbonCopy})
          """.stripMargin)
          .on('userid -> userid,
              'send -> send,
              'carbonCopy -> carbonCopy,
              'blindCarbonCopy -> blindCarbonCopy).executeInsert()
    }
    Cache.set(idCacheKey + userid, getSetByUser(userid))
  }

  def deleteByUser(userid: Long) = {
    DB.withConnection {
      implicit c =>
        SQL(
          """
          delete from emailSet where userid = {userid}
          """.stripMargin)
          .on('userid -> userid).executeUpdate()
    }
  }

  def getSetByUser(userid: Long) = {
    DB.withConnection { implicit c =>
      SQL("select * from emailSet where userid={userid}")
        .on('userid -> userid).as(simple.singleOpt)
    }
  }

  def getSetByUserFromCache(userid: Long) = {
    Cache.getOrElse[Option[EmailSet]](idCacheKey + userid) {
      getSetByUser(userid)
    }
  }

  def getUsersFromSet(user: User) = {
    val emailset = EmailSet.getSetByUserFromCache(user.id)

    var touserlist: List[User] = Nil
    var ccuserlist: List[User] = Nil
    var bccuserlist: List[User] = Nil
    if(emailset.nonEmpty){
      val touserStrings = emailset.get.send.split(";")
      val ccuserStrings = emailset.get.carbonCopy.split(";")
      val bccuserStrings = emailset.get.blindCarbonCopy.split(";")

      touserStrings.map(s =>
        if(User.getUserByEmail(s).nonEmpty)
          touserlist = User.getUserByEmail(s).get :: touserlist

      )

      ccuserStrings.map(s =>
        if(User.getUserByEmail(s).nonEmpty)
          ccuserlist = User.getUserByEmail(s).get :: ccuserlist

      )

      bccuserStrings.map(s =>
        if(User.getUserByEmail(s).nonEmpty)
          bccuserlist = User.getUserByEmail(s).get :: bccuserlist

      )
    }else{
      touserlist = user :: touserlist
    }

    Tuple3(touserlist, ccuserlist, bccuserlist)
  }
}