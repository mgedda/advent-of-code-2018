package day04

import scala.io.Source

object Day04_1
{
  case class Sleep(guardId: Int, day: String, minutes: Seq[Int])

  case class GuardMinutes(guardId: Int, minutes: Seq[Int])

  def getSleeps(records: Seq[String]) : Seq[Sleep] =
  {
    val recordsSorted = records.sorted
    var guardId = -1
    var sleeps: Seq[Sleep] = Seq()

    for (i <- recordsSorted.indices)
    {
      val record = recordsSorted(i)
      if (record.contains("Guard"))
      {
        // New guard begins shift
        guardId = record.split(" ")(3).substring(1).toInt
      }

      if (record.contains("falls asleep"))
      {
        val day = record.split(" ")(0).substring(1)
        val startMinute = record.split(" ")(1).substring(3, 5).toInt
        val endMinute = recordsSorted(i + 1).split(" ")(1).substring(3, 5).toInt

        sleeps = sleeps ++ Seq(Sleep(guardId, day, startMinute until endMinute))
      }
    }

    sleeps
  }

  def strategy1(records: Seq[String]) : Int =
  {
    val sleeps = getSleeps(records)
    val guardMinutes = sleeps.groupBy(_.guardId).map(gs => GuardMinutes(gs._1, gs._2.flatMap(_.minutes)))

    val maxEntry = guardMinutes.maxBy(_.minutes.size)
    val minute = maxEntry.minutes.groupBy(identity).maxBy(_._2.size)._1

    maxEntry.guardId * minute
  }


  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      assert(strategy1(Seq(
        "[1581-11-01 00:25] wakes up",
        "[1581-11-01 00:00] Guard #10 begins shift",
        "[1581-11-01 00:55] wakes up",
        "[1581-11-01 00:05] falls asleep",
        "[1581-11-01 23:58] Guard #99 begins shift",
        "[1581-11-02 00:50] wakes up",
        "[1581-11-03 00:05] Guard #10 begins shift",
        "[1581-11-01 00:30] falls asleep",
        "[1581-11-03 00:29] wakes up",
        "[1581-11-02 00:40] falls asleep",
        "[1581-11-05 00:03] Guard #99 begins shift",
        "[1581-11-04 00:36] falls asleep",
        "[1581-11-03 00:24] falls asleep",
        "[1581-11-05 00:45] falls asleep",
        "[1581-11-04 00:02] Guard #99 begins shift",
        "[1581-11-04 00:46] wakes up",
        "[1581-11-05 00:55] wakes up")) == 10 * 24)
    }

    // Read input
    //
    val patches = Source.fromResource("day04.txt").getLines.toSeq

    println(strategy1(patches))
  }

}
