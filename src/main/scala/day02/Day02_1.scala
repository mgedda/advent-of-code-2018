package day02

import scala.io.Source

object Day02_1
{

  def checksum(ids: Seq[String]) : Int =
  {
    processId(ids, 0, 0)
  }

  def processId(ids: Seq[String], twos: Int, threes: Int) : Int =
  {
    if (ids.isEmpty)
      twos * threes
    else
    {
      val charMap = ids.head.toSeq.groupBy(_.charValue())
      val newTwos = charMap.values.map(v => (v.length - 2 == 0).compare(false)).sum > 0 compare false
      val newThrees = charMap.values.map(v => (v.length - 3 == 0).compare(false)).sum > 0 compare false
      processId(ids.tail, twos + newTwos, threes + newThrees)
    }
  }

  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      assert(checksum(Seq("abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab")) == 12)
    }

    // Read input
    //
    val ids = Source.fromResource("day02.txt").getLines.toSeq

    println(checksum(ids))
  }

}
