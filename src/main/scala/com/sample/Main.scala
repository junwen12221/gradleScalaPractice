package com.sample

import java.util.{Calendar, Date}

class DateHelper (number: Int) {
  def days(when: String): Date = {
    val date = Calendar.getInstance()
    when match {
      case DateHelper.ago=>date.add(Calendar.DAY_OF_MONTH,-number)
      case DateHelper.from_now=>date.add(Calendar.DAY_OF_MONTH,number)
      case _=>date
    }
    date.getTime
  }
  def days2(when: String): DateHelper = {
    val date = Calendar.getInstance()
    when match {
      case DateHelper.ago=>date.add(Calendar.DAY_OF_MONTH,-number)
      case DateHelper.from_now=>date.add(Calendar.DAY_OF_MONTH,number)
      case _=>date
    }
   this
  }
}

object DateHelper {
  val ago = "ago";
  val from_now = "from_now";

  implicit def convertInt2Date(number: Int) = new DateHelper(number);
}
import DateHelper._
object Main {
  def main(args: Array[String]): Unit = {
    val twoDayAgo=2 days2 ago days ago;
    println("Hello World")
    Util.print()
    Util.print()
  }
}