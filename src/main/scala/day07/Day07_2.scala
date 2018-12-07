package day07

import scala.io.Source

object Day07_2
{

  def countTraverseTime(workers: Int, stepDuration: Int, instructions: Seq[String]) : Int =
  {
    val connections = instructions.map(s => (s.split(" ")(1), s.split(" ")(7)))
    val forwardDependencies = connections.groupBy(_._1).map(d => (d._1, d._2.map(_._2))).withDefaultValue(List())
    val backwardDependencies = connections.groupBy(_._2).map(d => (d._1, d._2.map(_._1)))

    val possibleStarts = connections.map(_._1)
    val available = possibleStarts.filterNot(c => backwardDependencies.keys.toSet.contains(c)).toSet

    followRec(workers, stepDuration, available, Seq(), Seq(), 0, forwardDependencies, backwardDependencies)
  }


  def followRec(workers: Int,
                stepDuration: Int,
                available: Set[String],
                traversed: Seq[String],
                inProgress: Seq[(String, Int)],
                time: Int,
                forwardDependencies: Map[String, Seq[String]],
                backwardDependencies: Map[String, Seq[String]]) : Int =
  {
    val finishedConnections = inProgress.filter(p => p._2 == time).map(_._1)
    val updatedTraversed = traversed ++ finishedConnections.sorted
    val possibleUnlocks = finishedConnections.flatMap(forwardDependencies(_)).toSet
    val unlocked = possibleUnlocks.filter(p => backwardDependencies(p).toSet.subsetOf(updatedTraversed.toSet))

    val availableInclUnlocked = available ++ unlocked
    val inProgressWithFinishedRemoved = inProgress.filterNot(p => p._2 == time)

    if (availableInclUnlocked.isEmpty && inProgressWithFinishedRemoved.isEmpty)
      time
    else
    {
      val freeWorkers = workers - inProgressWithFinishedRemoved.length
      val nextConnections = availableInclUnlocked.toSeq.sorted.take(math.min(freeWorkers, availableInclUnlocked.size))
      val updatedAvailable = availableInclUnlocked -- nextConnections

      val base = 'A'.toInt - 1
      val nextConnectionsWithFinishTimes = nextConnections.map(c => (c, c(0).toInt - base + stepDuration + time))

      val updatedInProgress = inProgressWithFinishedRemoved ++ nextConnectionsWithFinishTimes.toSet
      val updatedTime = updatedInProgress.minBy(_._2)._2

      followRec(workers, stepDuration, updatedAvailable, updatedTraversed, updatedInProgress, updatedTime,
        forwardDependencies, backwardDependencies)
    }
  }


  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      assert(countTraverseTime(2, 0, Seq(
        "Step C must be finished before step A can begin.",
        "Step C must be finished before step F can begin.",
        "Step A must be finished before step B can begin.",
        "Step A must be finished before step D can begin.",
        "Step B must be finished before step E can begin.",
        "Step D must be finished before step E can begin.",
        "Step F must be finished before step E can begin.")) == 15)
    }

    // Read input
    //
    val instructions = Source.fromResource("day07.txt").getLines.toSeq

    println(countTraverseTime(5, 60, instructions))
  }

}
