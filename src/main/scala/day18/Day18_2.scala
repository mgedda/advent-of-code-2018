package day18


import scala.io.Source

object Day18_2
{

  def toIndexedSeq(areaScans: Seq[String]): IndexedSeq[Char] =
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
    a.toIndexedSeq
  }

  def resourceValue(areaScans: Seq[String], iterations: Int): Int =
  {
    val xs: Int = areaScans.head.length
    val ys: Int = areaScans.length
    val lumberArea = toIndexedSeq(areaScans)

    val (cycleFrom, cycleUntil, visitedStates) = iterateRec(lumberArea, 0, iterations, xs, ys, Map())

    val cycleSize = cycleUntil - cycleFrom
    val i = cycleFrom + (iterations - cycleFrom) % cycleSize
    val invertedMap = visitedStates.groupBy(_._2).mapValues(_.head._1)
    val finalState = invertedMap(i)

    finalState.count(_ == '#') * finalState.count(_ == '|')
  }

  def iterateRec(lumberArea: IndexedSeq[Char],
                 count: Int,
                 iterations: Int,
                 xs: Int,
                 ys: Int,
                 visitedStates: Map[IndexedSeq[Char], Int]): (Int, Int, Map[IndexedSeq[Char], Int]) =
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

    val updatedLumberArea = lumberArea.zipWithIndex.par.map(getChar).toIndexedSeq

    if (visitedStates.contains(updatedLumberArea))
      (visitedStates(updatedLumberArea), count + 1, visitedStates)
    else
    {
      iterateRec(updatedLumberArea, count + 1, iterations, xs, ys, visitedStates + (updatedLumberArea -> (count + 1)))
    }
  }

  def main(args: Array[String]): Unit =
  {
    // Read input
    //
    val input = Source.fromResource("day18.txt").getLines.toSeq

    print(resourceValue(input, 1000000000))
  }

}
