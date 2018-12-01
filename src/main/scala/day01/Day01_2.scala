package day01

import scala.io.Source

object Day01_2
{

  def computeRepeatedFrequency(freqChangesAsStrings: Seq[String]) : Int =
  {
    val freqChanges = freqChangesAsStrings.map(_.toInt)
    changeFrequency(freqChanges.head, Set(0), 1, freqChanges)
  }

  def changeFrequency(frequency: Int, visited: Set[Int], counter: Int, freqChanges: Seq[Int]) : Int =
  {
    if (visited.contains(frequency))
      frequency
    else
      changeFrequency(frequency + freqChanges(counter % freqChanges.size), visited + frequency, counter + 1, freqChanges)
  }


  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      assert(computeRepeatedFrequency(Seq("+1", "-2", "+3", "+1")) == 2)
      assert(computeRepeatedFrequency(Seq("+1", "-1")) == 0)
      assert(computeRepeatedFrequency(Seq("+3", "+3", "+4", "-2", "-4")) == 10)
      assert(computeRepeatedFrequency(Seq("-6", "+3", "+8", "+5", "-6")) == 5)
      assert(computeRepeatedFrequency(Seq("+7", "+7", "-2", "-7", "-4")) == 14)
    }

    // Read input
    //
    val freqChanges = Source.fromResource("day01.txt").getLines.toSeq

    println(computeRepeatedFrequency(freqChanges))
  }

}
