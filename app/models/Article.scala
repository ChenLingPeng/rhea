package models

import anorm.SqlParser._
import anorm._
import play.api.Play.current
import play.api.db.DB

/**
 * Created by chenlingpeng on 2015/1/14.
 */
case class Article(id: Long, uid: Long, ctime: Long, title: String, content: String)

object Article{
  val simple = {
    get[Long]("article.id") ~
      get[Long]("article.uid") ~
      get[Long]("article.ctime") ~
      get[String]("article.title") ~
      get[String]("article.content") map { case id ~ uid ~ ctime ~ title ~ content =>
      Article(id, uid, ctime, title, content)
    }
  }

  def getById(id: Long) = {
    DB.withConnection{implicit c=>
    SQL("select * from article where id={id}").on('id->id).as(simple.single)
    }
  }

  def createArticle(uid: Long, title: String, content: String) = {
    DB.withConnection { implicit c => {
      val aid:Option[Long] = SQL(
        "insert into article (uid, ctime, title, content) values ({uid},{ctime},{title},{content})")
        .on(
          'uid -> uid,
          'ctime -> System.currentTimeMillis(),
          'title -> title,
          'content -> content
        ).executeInsert()
      aid.get
    }
    }
  }

  def list() = {
    DB.withConnection { implicit c => {
      SQL("select * from article").as(simple *)
    }
    }
  }
}
