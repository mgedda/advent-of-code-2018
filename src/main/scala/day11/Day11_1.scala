package day11

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Day11_1
{
  case class GridSum(topLeft: (Int, Int), sum: Int)

  case class FuelGrid(xs: Int, ys: Int, serial: Int)
  {
    val array: ArrayBuffer[Int] = {
      val a = scala.collection.mutable.ArrayBuffer.fill(xs * ys)(0)
      for (y <- 0 until ys)
      {
        for (x <- 0 until xs)
        {
          a(x + y * xs) = computePower(x, y, serial)
        }
      }
      a
    }

    def getGridSums: Seq[GridSum] =
    {
      val coords = for {
        x <- 0 until xs - 2
        y <- 0 until xs - 2
      } yield (x, y)

      coords.map(c => {
        val x = c._1
        val y = c._2
        val sum = (0 until 3).flatMap(v => array.slice(x + (y + v) * xs, (x + 3) + (y + v) * xs)).sum
        GridSum(c, sum)
      })
    }
  }

  def computePower(x: Int, y: Int, serial: Int) : Int =
  {
    val rackId = x + 10
    val powerLevel = (rackId * y + serial) * rackId
    val digit = if (powerLevel < 100)
      0
    else
      (powerLevel / 100) % 10
    digit - 5
  }

  def largestTotalPower(serial: String) : (Int, Int) =
  {
    val fuelGrid = FuelGrid(300, 300, serial.toInt)
    fuelGrid.getGridSums.maxBy(_.sum).topLeft
  }


  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      assert(computePower(122, 79, 57) == -5)
      assert(computePower(217, 196, 39) == 0)
      assert(computePower(101, 153, 71) == 4)

      assert(largestTotalPower("18") == (33, 45))
      assert(largestTotalPower("42") == (21, 61))
    }

    // Read input
    //
    val gridSerial = Source.fromResource("day11.txt").getLines.toSeq.head

    print(largestTotalPower(gridSerial))
  }

}
