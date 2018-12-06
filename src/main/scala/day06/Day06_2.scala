package day06

import scala.io.Source

object Day06_2
{
  case class Matrix(locations: Seq[(Int, Int)], threshold: Int)
  {
    private val xs = locations.maxBy(_._1)._1 + 1
    private val ys = locations.maxBy(_._2)._2 + 1

    private val array: scala.collection.mutable.ArrayBuffer[Int] = {

      def dist(c1: (Int, Int), c2: (Int, Int)) : Int = math.abs(c1._1 - c2._1) + math.abs(c1._2 - c2._2)

      val a = scala.collection.mutable.ArrayBuffer.fill(xs * ys)(0)

      val coordinates = for {
        x <- 0 until xs
        y <- 0 until ys
      } yield (x, y)

      def assign(coord: (Int, Int)): Unit =
      {
        val totalDistance = locations.map(dist(_, coord)).sum
        a(coord._1 + coord._2 * xs) = if (totalDistance < threshold)
          totalDistance
        else
          -1
      }

      coordinates.foreach(assign)
      a
    }

    def getSafeAreaSize: Int = array.count(_ > -1)

    def print: Unit =
    {
      val upper = "abcdefghijklmnopqrstuvwxyz".toUpperCase.toSeq

      for (y <- 0 until ys)
      {
        for(x <- 0 until xs)
        {
          val label = array(x + y * xs)
          val c = {
            if (locations.contains((x, y)))
              upper(locations.indexOf((x, y)))
            else if (label == -1)
              "."
            else
              "#"
          }
          printf("%s ", c)
        }
        printf("\n")
      }
    }
  }


  def findSafeArea(coordinates: Seq[String], threshold: Int) : Int =
  {
    val intCoords = coordinates.map(c => (c.split(", ")(0).toInt, c.split(", ")(1).toInt))
    val m = Matrix(intCoords, threshold)
    //m.print
    m.getSafeAreaSize
  }


  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      assert(findSafeArea(Seq(
        "1, 1",
        "1, 6",
        "8, 3",
        "3, 4",
        "5, 5",
        "8, 9"), 32) == 16)
    }

    // Read input
    //
    val coordinates = Source.fromResource("day06.txt").getLines.toSeq

    println(findSafeArea(coordinates, 10000))
  }

}
