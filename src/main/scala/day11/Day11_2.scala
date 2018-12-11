package day11

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Day11_2
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

    def getGridSums(sz: Int): Seq[GridSum] =
    {
      for
      {
        x <- 0 until xs - (sz - 1)
        y <- 0 until xs - (sz - 1)
      } yield GridSum((x, y), (0 until sz).flatMap(v => array.slice(x + (y + v) * xs, (x + sz) + (y + v) * xs)).sum)
    }
  }

  def computePower(x: Int, y: Int, serial: Int) : Int =
  {
    val rackId = x + 10
    val powerLevel = (rackId * y + serial) * rackId
    (powerLevel / 100) % 10 - 5
  }

  def largestTotalPower(serial: String) : (Int, Int, Int) =
  {
    val fuelGrid = FuelGrid(300, 300, serial.toInt)
    val (maxGrid, sz) = (1 to 300).par.map(sz => (fuelGrid.getGridSums(sz).maxBy(_.sum), sz)).maxBy(_._1.sum)
    (maxGrid.topLeft._1, maxGrid.topLeft._2, sz)
  }


  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      assert(computePower(122, 79, 57) == -5)
      assert(computePower(217, 196, 39) == 0)
      assert(computePower(101, 153, 71) == 4)

      //assert(largestTotalPower("18") == (90, 269, 16))
      //assert(largestTotalPower("42") == (232, 251, 12))
    }

    // Read input
    //
    val gridSerial = Source.fromResource("day11.txt").getLines.toSeq.head

    print(largestTotalPower(gridSerial))
  }

}
