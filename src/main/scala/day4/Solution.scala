package day4

import java.time._
import java.time.format.DateTimeFormatter

import scala.collection.mutable

case class Event(time: String, description: String)

object Solution {

  def parse(str: String): Event = Event(str.substring(1, 17), str.substring(19))

  val DATE_FORMAT = DateTimeFormatter.ofPattern("HH:mm")

  def timeDelta(t1: String, t2: String): Long = (LocalTime.parse(t2.substring(11), DATE_FORMAT).toSecondOfDay - LocalTime.parse(t1.substring(11), DATE_FORMAT).toSecondOfDay) / 60

  def main(args: Array[String]): Unit = {
    val events = Data.data.split("\n").map(parse).sortBy(_.time).toSeq
    var currentGuard = "0"
    var asleepStartTime = ""
    var guardsAsleepTime = Map[String, Long]()

    def applyEvent: Event => Unit = e => {
      if (e.description.endsWith("begins shift")) {
        currentGuard = e.description.substring(e.description.indexOf("#"), e.description.indexOf(" begins shift"))
      }

      if (e.description.contains("falls asleep")) {
        asleepStartTime = e.time
      }

      if (e.description.contains("wakes up")) {
        val asleepTime = timeDelta(asleepStartTime, e.time)
        val oldAsleepTime: Long = guardsAsleepTime.getOrElse(currentGuard, 0)
        val totalAsleepTime = asleepTime + oldAsleepTime
        guardsAsleepTime = guardsAsleepTime + (currentGuard -> totalAsleepTime)
      }
    }

    events.foreach(applyEvent)
    val guardAsleepMost = guardsAsleepTime.toStream.maxBy(_._2)._1
    println(guardsAsleepTime.toStream.sortBy(- _._2).toList)
    println(guardAsleepMost)

    var minutesAsleep = mutable.HashMap[Int, Long]().withDefaultValue(0)

    def trackGuardsMinutes: Event => Unit = e => {
      if (e.description.endsWith("begins shift")) {
        currentGuard = e.description.substring(e.description.indexOf("#"), e.description.indexOf(" begins shift"))
      }

      if (e.description.contains("falls asleep")) {
        asleepStartTime = e.time
      }

      if (e.description.contains("wakes up") && guardAsleepMost.equals(currentGuard)) {
        val asleepStartMinute = Integer.parseInt(asleepStartTime.substring(14))
        val asleepEndMinute = Integer.parseInt(e.time.substring(14))

        for (i <- asleepStartMinute until asleepEndMinute) {
          val oldDate: Long = minutesAsleep(i)
          minutesAsleep = minutesAsleep + (i -> (oldDate + 1))
        }
      }
    }

    events.foreach(trackGuardsMinutes)
    println(minutesAsleep.toStream.sortBy(- _._2).toList)
    println(minutesAsleep.toStream.maxBy(_._2)._1)

    println(Integer.parseInt(guardAsleepMost.substring(1)) * minutesAsleep.toStream.maxBy(_._2)._1)

    //Strategy 2

    var minutesAsleepPerGuard = mutable.HashMap[String, Long]().withDefaultValue(0)
    def trackGuardsMinutesPerGuard: Event => Unit = e => {
      if (e.description.endsWith("begins shift")) {
        currentGuard = e.description.substring(e.description.indexOf("#"), e.description.indexOf(" begins shift"))
      }

      if (e.description.contains("falls asleep")) {
        asleepStartTime = e.time
      }

      if (e.description.contains("wakes up")) {
        val asleepStartMinute = Integer.parseInt(asleepStartTime.substring(14))
        val asleepEndMinute = Integer.parseInt(e.time.substring(14))

        for (i <- asleepStartMinute until asleepEndMinute) {
          val oldDate: Long = minutesAsleepPerGuard(currentGuard + "-" + i.toString)
          minutesAsleepPerGuard = minutesAsleepPerGuard + (currentGuard + "-" + i.toString -> (oldDate + 1))
        }
      }
    }

    events.foreach(trackGuardsMinutesPerGuard)
    val result = minutesAsleepPerGuard.toStream.maxBy(_._2)._1
    println(result)
  }
}
