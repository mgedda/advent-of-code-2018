package day03

import scala.io.Source

object Day03_1
{

  def overlaps(patches: Seq[String]) : Int =
  {
    overlapsRec(patches, Set(), Set())
  }

  def overlapsRec(patches: Seq[String], visited: Set[(Int, Int)], overlap: Set[(Int, Int)]) : Int =
  {
    if (patches.isEmpty)
      overlap.size
    else
    {
      val tokens = patches.head.split(" ")
      val x = tokens(2).split(",")(0).toInt
      val y = tokens(2).split(",")(1).stripSuffix(":").toInt
      val xs = tokens(3).split("x")(0).toInt
      val ys = tokens(3).split("x")(1).toInt

      val coords = for {
        x_ <- List.range(x, x + xs)
        y_ <- List.range(y, y + ys)
      } yield (x_, y_)

      val overlaps = coords.filter(c => visited.contains(c))

      overlapsRec(patches.tail, visited ++ coords.toSet, overlap ++ overlaps.toSet)
    }
  }

  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      assert(overlaps(Seq(
        "#1 @ 1,3: 4x4",
        "#2 @ 3,1: 4x4",
        "#3 @ 5,5: 2x2")) == 4)
    }

    // Read input
    //
    val patches = Source.fromResource("day03.txt").getLines.toSeq

    println(overlaps(patches))
  }

}
