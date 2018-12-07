package day07

import scala.io.Source

object Day07_1
{

  def findOrder(instructions: Seq[String]) : String =
  {
    val connections = instructions.map(s => (s.split(" ")(1), s.split(" ")(7)))
    val forwardDependencies = connections.groupBy(_._1).map(d => (d._1, d._2.map(_._2)))
    val backwardDependencies = connections.groupBy(_._2).map(d => (d._1, d._2.map(_._1)))

    val possibleStarts = connections.map(_._1)
    val available = possibleStarts.filterNot(c => backwardDependencies.keys.toSet.contains(c)).toSet

    followRec(available, Seq(), forwardDependencies, backwardDependencies)
  }


  def followRec(available: Set[String],
                traversed: Seq[String],
                forwardDependencies: Map[String, Seq[String]],
                backwardDependencies: Map[String, Seq[String]]) : String =
  {
    if (available.isEmpty)
      traversed.mkString
    else
    {
      val next = available.toSeq.sorted.head
      if (forwardDependencies.contains(next))
      {
        val possibleUnlocks = forwardDependencies(next)
        val unlocks = possibleUnlocks.filter(p => backwardDependencies(p).toSet.subsetOf(traversed.toSet ++ Set(next)))
        followRec(available.filterNot(_ == next) ++ unlocks.toSet, traversed ++ Seq(next), forwardDependencies, backwardDependencies)
      }
      else
        followRec(available.filterNot(_ == next), traversed ++ Seq(next), forwardDependencies, backwardDependencies)
    }
  }

  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      assert(findOrder(Seq(
        "Step C must be finished before step A can begin.",
        "Step C must be finished before step F can begin.",
        "Step A must be finished before step B can begin.",
        "Step A must be finished before step D can begin.",
        "Step B must be finished before step E can begin.",
        "Step D must be finished before step E can begin.",
        "Step F must be finished before step E can begin.")) == "CABDFE")
    }

    // Read input
    //
    val instructions = Source.fromResource("day07.txt").getLines.toSeq

    println(findOrder(instructions))
  }

}
