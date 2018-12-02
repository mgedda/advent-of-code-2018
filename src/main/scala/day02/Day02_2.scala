package day02

import scala.io.Source

object Day02_2
{

  def differByOneChar(id1: String, id2: String) : Boolean =
  {
    id1.toSeq.zip(id2.toSeq).map(cc => cc._1 != cc._2 compare false).sum == 1
  }

  def composeStringOfCommonChars(id1: String, id2: String) : String =
  {
    id1.toSeq.zip(id2.toSeq).filter(cc => cc._1 == cc._2).unzip._1.mkString
  }

  def getIdChars(ids: Seq[String]) : String =
  {
    val uniquePairs = for {
      (x, idxX) <- ids.zipWithIndex
      (y, idxY) <- ids.zipWithIndex
      if idxX < idxY
    } yield (x, y)

    val pair = uniquePairs.filter(p => differByOneChar(p._1, p._2)).head

    composeStringOfCommonChars(pair._1, pair._2)
  }


  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      assert(getIdChars(Seq("abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz")) == "fgij")
    }

    // Read input
    //
    val ids = Source.fromResource("day02.txt").getLines.toSeq

    println(getIdChars(ids))
  }

}
