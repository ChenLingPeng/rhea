package util

import java.util

import scala.collection.mutable.ArrayBuffer

/**
 * Created by chenlingpeng on 2014/10/27.
 */
class Test extends App{
  def filter(page: Int, uid: Long, projectid: Long, groupid: Long, state: Int) = {
    val sql = new StringBuilder("select * from task ")
    val params = new ArrayBuffer[String]()

    if (uid > 0 || projectid > 0 || groupid > 0 || state > 0) {
      sql.append("where ")
      if(uid>0){
        params+= "ownby={ownby}"
      }
      if(projectid>0){
        params+= "projectid={projectid}"
      }
      if(groupid>0){
        params+= "groupid={groupid}"
      }
      if(state>0){
        params+= "state={state}"
      }
      sql.append(params.toArray.mkString(" and "))
    }
    sql.append(" limit 10 offset {offset}")


  }

  filter(1,3,4,5,3)
}
