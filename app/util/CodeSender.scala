package util

import java.io.File
import java.util.Calendar
import java.util.Date;
import java.text.SimpleDateFormat;
import com.typesafe.plugin._
import play.api.{Logger, Play}
import play.api.Play.current
import play.api.libs.concurrent.Akka
import models.EmailSet

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import models.{Task, User}


class CodeSender {

  protected val mailer: MailerAPI = use[MailerPlugin].email

  protected def delayedExecution(block: => Unit): Unit = Akka.system.scheduler.scheduleOnce(1.seconds)(block)

  def sendCode(code: String, user: User) = {
    delayedExecution {
      Logger.info(s"${user.email} want reset password")
      mailer.setSubject("Password Reset")
      mailer.setRecipient(user.email)
      mailer.setFrom("Rhea Admin <rhea@neotel.com.cn>")
      mailer.sendHtml(views.html.email_template(code, user.id, Play.configuration.getString("deloyhost").get).body)
    }
  }

//  val dayOfWeek = List[String]("星期日", "星期一", "星期二", "星期三", "星期四", "星期五", "星期六")

  def sendReport(user: User, otherjob: String, helpcontent:String, file: File = null, filename: String = null, filetype: String = null) = {
    delayedExecution {
      Logger.info(s"${user.email} send report")
      val report = Task.weeklyReport(user)
      //计算周报标题
      val sdf = new SimpleDateFormat("yyyy.MM")
      val titleCalendar = Calendar.getInstance()
      titleCalendar.setFirstDayOfWeek(Calendar.MONDAY)
      titleCalendar.set(Calendar.HOUR_OF_DAY, 0)
      titleCalendar.set(Calendar.MINUTE, 0)
      titleCalendar.set(Calendar.SECOND, 0)
      titleCalendar.set(Calendar.MILLISECOND, 0)

      titleCalendar.set(Calendar.DAY_OF_WEEK, Calendar.FRIDAY)
      val fridayThisWeek = titleCalendar.getTimeInMillis
      titleCalendar.set(Calendar.DAY_OF_MONTH , 1)
      val firstDay = titleCalendar.getTimeInMillis

      val week = (fridayThisWeek - firstDay)/(7*24*3600*1000) + 1
      val month = sdf.format(new Date(fridayThisWeek))

      val emailset = EmailSet.getSetByUserFromCache(user.id)
      if(emailset.nonEmpty){
        if(!emailset.get.send.isEmpty){
            mailer.setRecipient(emailset.get.send.split(";"):_*)
        }
        if(!emailset.get.carbonCopy.isEmpty){
          mailer.setCc(emailset.get.carbonCopy.split(";"):_*)
        }
        if(!emailset.get.blindCarbonCopy.isEmpty){
          mailer.setBcc(emailset.get.blindCarbonCopy.split(";"):_*)
        }
      }

      mailer.setSubject(s"工作报告.${user.name}.$month.第${week}周")
      if(emailset.isEmpty){
        mailer.setRecipient(user.email)
      }
      if(file != null){
        mailer.addAttachment(filename, file, filetype)
      }

      mailer.setFrom(user.name + " <rhea@neotel.com.cn>")
      println(otherjob)
      println(helpcontent)
      mailer.sendHtml(views.html.copypage(user,report._1.toMap,report._2.toMap,report._3.toMap,report._4.toMap,helpcontent).body.replace("_OTHERJOB_",otherjob))

    }
  }
}

object CodeSender extends CodeSender
