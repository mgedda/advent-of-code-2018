package day20


import scala.io.Source

object Day20_1
{

  case class Vec(x: Int, y: Int)
  {
    def +(v: Vec): Vec = Vec(x + v.x, y + v.y)
  }

  def getDir(c: Char): Vec = {
    c match {
      case 'E' => Vec(1, 0)
      case 'W' => Vec(-1, 0)
      case 'S' => Vec(0, -1)
      case 'N' => Vec(0, 1)
    }
  }

  def findMinDoors(regex: String): Int =
  {
    assert(regex.head == '^' && regex.last == '$')

    val shortestPaths = buildShortestPaths(regex, 0, Seq(Vec(0,0)), Map())
    shortestPaths.maxBy(_._2.length)._2.length - 1
  }

  def getForks(regex: String, i: Int, currString: String, totalString: String, depth: Int, forks: Seq[String]): (String, Seq[String]) =
  {
    val c = regex(i)
    c match {
      case '(' => {
        if (depth == 0)
          getForks(regex, i + 1, "", totalString + c, depth + 1, forks)
        else
          getForks(regex, i + 1, currString + c, totalString + c, depth + 1, forks)
      }
      case ')' => {
        if (depth - 1 == 0)
          (totalString + c, forks ++ Seq(currString))
        else
          getForks(regex, i + 1, currString + c, totalString + c, depth - 1, forks)
      }
      case '|' => {
        if (depth - 1 == 0)
          getForks(regex, i + 1, "", totalString + c, depth, forks ++ Seq(currString))
        else
          getForks(regex, i + 1, currString + c, totalString + c, depth, forks)
      }
      case _ => getForks(regex, i + 1, currString + c, totalString + c, depth, forks)
    }
  }

  def buildShortestPaths(regex: String, i: Int, path: Seq[Vec], shortestPaths: Map[Vec, Seq[Vec]]): Map[Vec, Seq[Vec]] =
  {
    regex(i) match {
      case '^' => buildShortestPaths(regex, i + 1, path, shortestPaths) // start regex
      case '(' => { // fork
        val (substring, forks) = getForks(regex, i, "", "", 0, Seq())
        val r = substring.replace("(","\\(").replace("|","\\|").replace(")","\\)")
        val maps = forks.map(s => buildShortestPaths(regex.replaceFirst(r, s), i, path, shortestPaths))

        // Merge maps
        maps.flatMap(_.toSeq).groupBy(_._1).mapValues(_.map(_._2).toList.minBy(_.length))
      }
      case '$' => shortestPaths // end regex
      case _ => {  // 'E', 'W', 'N', 'S'
        val newPos = path.last + getDir(regex(i))
        val newPath = path ++ Seq(newPos)
        if (shortestPaths.contains(newPos) && shortestPaths(newPos).length <= newPath.length)
          shortestPaths  // terminate path since it is longer than previous paths to this position
        else
        {
          val updatedShortestPaths = if (shortestPaths.contains(newPos))
            shortestPaths.updated(newPos, newPath)
          else
            shortestPaths + (newPos -> newPath)

          buildShortestPaths(regex, i + 1, newPath, updatedShortestPaths)
        }
      }
    }
  }


  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      assert(getForks("^EE(NWS|ES(NW|)E|)EW", 3, "", "", 0, Seq()) == ("(NWS|ES(NW|)E|)", List("NWS", "ES(NW|)E", "")))
      assert(getForks("^(NW|)E", 1, "", "", 0, Seq()) == ("(NW|)", List("NW", "")))
      printf("Fork tests passed!\n")

      assert(findMinDoors("^WNE$") == 3)
      assert(findMinDoors("^ENWWW(NEEE|SSE(EE|N))$") == 10)
      assert(findMinDoors("^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$") == 18)
      assert(findMinDoors("^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$") == 23)
      assert(findMinDoors("^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$") == 31)

      printf("Tests passed\n")
    }

    // Read input
    //
    val input = Source.fromResource("day20.txt").getLines.toSeq.head

    print(findMinDoors(input))
  }

}
