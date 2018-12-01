package day01

import scala.io.Source

object Day01_1
{

  def computeFrequency(freqChanges: Seq[String]) : Int =
  {
    freqChanges.map(_.toInt).sum
  }


  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      assert(computeFrequency(Seq("0", "+1")) == 1)
      assert(computeFrequency(Seq("1", "-2")) == -1)
      assert(computeFrequency(Seq("-1", "+3")) == 2)
      assert(computeFrequency(Seq("2", "+1")) == 3)
      assert(computeFrequency(Seq("+1", "+1", "+1")) == 3)
      assert(computeFrequency(Seq("+1", "+1", "-2")) == 0)
      assert(computeFrequency(Seq("-1", "-2", "-3")) == -6)
    }

    // Read input
    //
    val freqChanges = Source.fromResource("day01.txt").getLines.toSeq

    println(computeFrequency(freqChanges))
  }

}
