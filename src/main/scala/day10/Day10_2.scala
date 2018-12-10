package day10

import scala.io.Source

object Day10_2
{

  case class Vector(x: Int, y: Int)
  {
    def +(v: Vector) = Vector(x + v.x, y + v.y)
  }

  case class Point(pos: Vector, vel: Vector)
  {
    def step : Point = Point(pos + vel, vel)
  }

  case class BoundingBox(xMin: Int, yMin: Int, xMax: Int, yMax: Int)
  {
    val xs: Long = xMax - xMin + 1
    val ys: Long = yMax - yMin + 1
    val size: Long = xs * ys
  }

  def getBoundingBox(points: Seq[Point]) : BoundingBox =
  {
    val x0 = points.minBy(_.pos.x).pos.x
    val y0 = points.minBy(_.pos.y).pos.y
    val x1 = points.maxBy(_.pos.x).pos.x
    val y1 = points.maxBy(_.pos.y).pos.y

    BoundingBox(x0, y0, x1, y1)
  }

  def printPoints(points: Seq[Point]) : Unit =
  {
    val bb = getBoundingBox(points)

    if (bb.size != bb.size.toInt)
      return  // bounding box too large

    val x0 = bb.xMin
    val y0 = bb.yMin

    val a = scala.collection.mutable.ArrayBuffer.fill(bb.size.toInt)(0)
    points.foreach(p => a((p.pos.x - x0 + (p.pos.y - y0) * bb.xs).toInt) = 1)

    for (y <- 0 until bb.ys.toInt)
    {
      for(x <- 0 until bb.xs.toInt)
      {
        if (a((x + y * bb.xs).toInt) > 0)
          print(" #")
        else
          print(" .")
      }
      printf("\n")
    }
    printf("\n")
  }

  def getSecondsRec(points: Seq[Point], boundingBox: BoundingBox, seconds: Int) : Int =
  {
    val updatedPoints = points.map(_.step)
    val updatedBoundingBox = getBoundingBox(updatedPoints)

    //printPoints(updatedPoints)
    //printf("BoundingBox xs=%d, ys=%d, size=%d\n", updatedBoundingBox.xs, updatedBoundingBox.ys, updatedBoundingBox.size)

    if (updatedBoundingBox.size > boundingBox.size)
      seconds
    else
      getSecondsRec(updatedPoints, updatedBoundingBox, seconds + 1)
  }

  def countSeconds(pointDefinitions: Seq[String]) : Int =
  {
    val points = pointDefinitions.map(d =>
    {
      val positions = d.split("position=<")(1).split(">")(0).trim.split(", ")
      val x = positions(0).trim.toInt
      val y = positions(1).trim.toInt
      val velocities = d.split("velocity=<")(1).split(">")(0).trim.split(", ")
      val vx = velocities(0).trim.toInt
      val vy = velocities(1).trim.toInt
      Point(Vector(x, y), Vector(vx, vy))
    })

    getSecondsRec(points, getBoundingBox(points), 0)
  }


  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      assert(countSeconds(Seq(
        "position=< 9,  1> velocity=< 0,  2>",
        "position=< 7,  0> velocity=<-1,  0>",
        "position=< 3, -2> velocity=<-1,  1>",
        "position=< 6, 10> velocity=<-2, -1>",
        "position=< 2, -4> velocity=< 2,  2>",
        "position=<-6, 10> velocity=< 2, -2>",
        "position=< 1,  8> velocity=< 1, -1>",
        "position=< 1,  7> velocity=< 1,  0>",
        "position=<-3, 11> velocity=< 1, -2>",
        "position=< 7,  6> velocity=<-1, -1>",
        "position=<-2,  3> velocity=< 1,  0>",
        "position=<-4,  3> velocity=< 2,  0>",
        "position=<10, -3> velocity=<-1,  1>",
        "position=< 5, 11> velocity=< 1, -2>",
        "position=< 4,  7> velocity=< 0, -1>",
        "position=< 8, -2> velocity=< 0,  1>",
        "position=<15,  0> velocity=<-2,  0>",
        "position=< 1,  6> velocity=< 1,  0>",
        "position=< 8,  9> velocity=< 0, -1>",
        "position=< 3,  3> velocity=<-1,  1>",
        "position=< 0,  5> velocity=< 0, -1>",
        "position=<-2,  2> velocity=< 2,  0>",
        "position=< 5, -2> velocity=< 1,  2>",
        "position=< 1,  4> velocity=< 2,  1>",
        "position=<-2,  7> velocity=< 2, -2>",
        "position=< 3,  6> velocity=<-1, -1>",
        "position=< 5,  0> velocity=< 1,  0>",
        "position=<-6,  0> velocity=< 2,  0>",
        "position=< 5,  9> velocity=< 1, -2>",
        "position=<14,  7> velocity=<-2,  0>",
        "position=<-3,  6> velocity=< 2, -1>"
      )) == 3)
    }

    // Read input
    //
    val instructions = Source.fromResource("day10.txt").getLines.toSeq

    print(countSeconds(instructions))
  }

}
