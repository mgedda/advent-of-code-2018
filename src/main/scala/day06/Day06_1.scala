package day06

import scala.io.Source

object Day06_1
{
  case class Matrix(locations: Seq[(Int, Int)])
  {
    private val xs = locations.maxBy(_._1)._1 + 1
    private val ys = locations.maxBy(_._2)._2 + 1

    private val array: scala.collection.mutable.ArrayBuffer[Int] = {

      def dist(c1: (Int, Int), c2: (Int, Int)) : Int = math.abs(c1._1 - c2._1) + math.abs(c1._2 - c2._2)

      def getLabel(coord: (Int, Int), indexedLocations: Seq[((Int, Int), Int)]) : Int =
      {
        val locSorted = indexedLocations.map(il => (il._2, dist(il._1, coord))).sortBy(_._2)
        if (locSorted.head._2 == locSorted.tail.head._2)
          -1
        else
          locSorted.head._1
      }

      val a = scala.collection.mutable.ArrayBuffer.fill(xs * ys)(0)

      val coordinates = for {
        x <- 0 until xs
        y <- 0 until ys
      } yield (x, y)

      val indexedLocations = locations.zipWithIndex

      def assign(coord: (Int, Int)): Unit =
      {
        val label = getLabel(coord, indexedLocations)
        a(coord._1 + coord._2 * xs) = label
      }

      coordinates.foreach(assign)

      a
    }

    def getLargestNonInfiniteArea: Int =
    {
      def getAreaSize(label: Int): Int =
      {
        val labelCoords = array.zipWithIndex.filter(_._1 == label).map(_._2).map(i => (i % xs, i / ys))
        if (labelCoords.minBy(_._1)._1 == 0 || labelCoords.minBy(_._2)._2 == 0
            || labelCoords.maxBy(_._1)._1 == xs - 1 || labelCoords.maxBy(_._2)._2 == ys - 1)
          -1   // infinite area
        else
          labelCoords.length
      }

      val areaSizes = locations.indices.map(getAreaSize)
      areaSizes.max
    }


    def print: Unit =
    {
      val lower = "abcdefghijklmnopqrstuvwxyz".toSeq
      val upper = lower.map(_.toUpper)

      for (y <- 0 until ys)
      {
        for(x <- 0 until xs)
        {
          val label = array(x + y * xs)
          val c = {
            if (label == -1)
              "."
            else if (locations.contains((x, y)))
              upper(label)
            else
              lower(label)
          }
          printf("%s ", c)
        }
        printf("\n")
      }
    }
  }


  def findLargestArea(coordinates: Seq[String]) : Int =
  {
    val intCoords = coordinates.map(c => (c.split(", ")(0).toInt, c.split(", ")(1).toInt))
    val m = Matrix(intCoords)
    //m.print
    m.getLargestNonInfiniteArea
  }


  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      assert(findLargestArea(Seq(
        "1, 1",
        "1, 6",
        "8, 3",
        "3, 4",
        "5, 5",
        "8, 9")) == 17)
    }

    // Read input
    //
    val coordinates = Source.fromResource("day06.txt").getLines.toSeq

    println(findLargestArea(coordinates))
  }

}
