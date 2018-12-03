package day03

import scala.io.Source

case class Claim(patch: String)
{
  val id = patch.split(" ").head.substring(1).toInt

  val coords = {
    val tokens = patch.split(" ")
    val x = tokens(2).split(",")(0).toInt
    val y = tokens(2).split(",")(1).stripSuffix(":").toInt
    val xs = tokens(3).split("x")(0).toInt
    val ys = tokens(3).split("x")(1).toInt

    val cs = for {
      x_ <- List.range(x, x + xs)
      y_ <- List.range(y, y + ys)
    } yield (x_, y_)

    cs.toSet
  }
}

object Day03_2
{

  def noOverlap(claim: Claim, other: Seq[Claim]) : Boolean =
  {
    val overlappingClaims = other.filter(c2 => claim.coords.exists(c2.coords.contains))
    overlappingClaims.isEmpty
  }

  def findClaimWithNoOverlap(patches: Seq[String]) : Int =
  {
    val claims = patches.map(Claim)
    claims.filter(c1 => noOverlap(c1, claims.filter(_ != c1))).head.id
  }

  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      assert(findClaimWithNoOverlap(Seq(
        "#1 @ 1,3: 4x4",
        "#2 @ 3,1: 4x4",
        "#3 @ 5,5: 2x2")) == 3)
    }

    // Read input
    //
    val patches = Source.fromResource("day03.txt").getLines.toSeq

    println(findClaimWithNoOverlap(patches))
  }

}
