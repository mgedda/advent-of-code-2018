package day18


import scala.io.Source

object Day18_1
{

  def toArray(areaScans: Seq[String]): Array[Char] =
  {
    val xs: Int = areaScans.head.length
    val ys: Int = areaScans.length

    val a = scala.collection.mutable.ArrayBuffer.fill(xs * ys)('.')
    for (y <- areaScans.indices)
    {
      for (x <- areaScans.head.indices)
      {
        a(x + y * xs) = areaScans(y)(x)
      }
    }
    a.toArray
  }

  def resourceValue(areaScans: Seq[String], iterations: Int): Int =
  {
    val xs: Int = areaScans.head.length
    val ys: Int = areaScans.length
    val lumberArea = toArray(areaScans)

    iterateRec(lumberArea, 0, iterations, xs, ys)
  }

  def iterateRec(lumberArea: Array[Char], count: Int, iterations: Int, xs: Int, ys: Int): Int =
  {
    def getChar(pair: (Char, Int)): Char =
    {
      def adjacentIndices(i: Int): Seq[Int] =
      {
        val x = i % xs
        val y = i / xs

        val coords = for {
          py <- y - 1 to y + 1
          px <- x - 1 to x + 1
          if (px, py) != (x, y)
        } yield (px, py)

        coords.filter(p => p._1 >= 0 && p._1 < xs && p._2 >= 0 && p._2 < ys).map(p => p._1 + p._2 * xs)
      }

      val (c, i) = pair
      val adjacentAcres = adjacentIndices(i).collect(lumberArea)
      c match {
        case '.' => if (adjacentAcres.count(_ == '|') >= 3) '|' else '.'
        case '|' => if (adjacentAcres.count(_ == '#') >= 3) '#' else '|'
        case '#' => if (adjacentAcres.count(_ == '#') >= 1 && adjacentAcres.count(_ == '|') >= 1) '#' else '.'
      }
    }

    if (count == iterations)
      lumberArea.count(_ == '#') * lumberArea.count(_ == '|')
    else
    {
      val updatedLumberArea = lumberArea.zipWithIndex.map(getChar)
      iterateRec(updatedLumberArea, count + 1, iterations, xs, ys)
    }
  }

  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      assert(resourceValue(Seq(
        ".#.#...|#.",
        ".....#|##|",
        ".|..|...#.",
        "..|#.....#",
        "#.#|||#|#|",
        "...#.||...",
        ".|....|...",
        "||...#|.#|",
        "|.||||..|.",
        "...#.|..|."), 10) == 37 * 31)

      printf("Tests passed\n")
    }

    // Read input
    //
    val input = Source.fromResource("day18.txt").getLines.toSeq

    print(resourceValue(input, 10))
  }

}
