package util

import java.util.Calendar

import com.twitter.util.{Time, TimeFormat}

/**
 * Created by chenlingpeng on 2014/10/28.
 */
object TimeFormatUtil {
  def toMilliseconds(date: String, format: String = "yyyy-MM-dd HH:mm:ss"): Long = {
    new TimeFormat(format).parse(date).inMilliseconds - 8 * 60 * 60 * 1000
  }

  def toLocalDate(date: Long, format: String = "yyyy-MM-dd HH:mm:ss"): String = {
    new TimeFormat(format).format(Time.fromMilliseconds(date + 8 * 60 * 60 * 1000))
  }

  def toReadableFormat(date: Long): String = {
    val delta = System.currentTimeMillis() - date
    delta match {
      case rightNow if rightNow < 1000 =>
        "刚刚"
      case seconds if seconds < 60 * 1000 =>
        seconds / 1000 + "秒前"
      case minutes if minutes < 60 * 60 * 1000 =>
        minutes / (60 * 1000) + "分钟前"
      case hours if hours < 24 * 60 * 60 * 1000 =>
        hours / (60 * 60 * 1000) + "小时前"
      case days =>
        days / (24 * 60 * 60 * 1000) + "天前"
    }
  }

  def firstDayOfWeek = {
    val calendar = Calendar.getInstance()
    calendar.setFirstDayOfWeek(Calendar.MONDAY)
    calendar.set(Calendar.DAY_OF_WEEK, Calendar.MONDAY)
    calendar.set(Calendar.HOUR_OF_DAY, 0)
    calendar.set(Calendar.MINUTE,0)
    calendar.set(Calendar.SECOND,0)
    calendar.set(Calendar.MILLISECOND,0)
    calendar.getTimeInMillis-8 * 60 * 60 * 1000
  }
}
