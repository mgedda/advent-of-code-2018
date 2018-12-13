package day13

import scala.io.Source

object Day13_1
{

  case class Vector(x: Int, y:Int)
  {
    def +(v: Vector): Vector = Vector(x + v.x, y + v.y)
    def *(v: Vector): Vector = Vector(x * v.x, y * v.y)
    def rotate(theta: Double): Vector = {
      val px = x * math.cos(theta) - y * math.sin(theta)
      val py = x * math.sin(theta) + y * math.cos(theta)
      Vector(px.toInt, py.toInt)
    }
  }

  case class Cart(pos: Vector, dir: Vector, crossings: Int)
  {
    def move(from: Char): Cart = {
      from match {
        case '-' => goStraight(0)
        case '|' => goStraight(0)
        case '\\' => {
          if (dir.y != 0)
            goLeft(0)
          else
            goRight(0)
        }
        case '/' => {
          if (dir.y != 0)
            goRight(0)
          else
            goLeft(0)
        }
        case '+' => handleCrossing()
        //case _ => Cart(pos, dir, crossings)   // dummy, should never happen
      }
    }

    def goStraight(cross: Int): Cart = Cart(pos + dir, dir, crossings + cross)
    def goLeft(cross: Int): Cart = {
      val newDir = dir.rotate(-math.Pi / 2.0)
      Cart(pos + newDir, newDir, crossings + cross)
    }
    def goRight(cross: Int): Cart = {
      val newDir = dir.rotate(math.Pi / 2.0)
      Cart(pos + newDir, newDir, crossings + cross)
    }

    val crossingActions = Seq(goLeft(_), goStraight(_), goRight(_))

    def handleCrossing(): Cart = {
      crossingActions(crossings % crossingActions.length)(1)
    }

    def getChar(): Char =
    {
      dir match {
        case Vector(1, 0) => '>'
        case Vector(-1, 0) => '<'
        case Vector(0, 1) => 'v'
        case Vector(0, -1) => '^'
      }
    }
  }

  case class Track(rows: Seq[String])
  {
    def get(pos: Vector): Char = rows(pos.y)(pos.x)
    def printWithCarts(carts: Seq[Cart]): Unit = {
      for (y <- rows.indices)
      {
        for (x <- 0 until rows.head.length)
        {
          val cartsAtPosition = carts.filter(_.pos == Vector(x, y))
          if (cartsAtPosition.length > 1)
            print("X")
          else if (cartsAtPosition.length == 1)
            printf("%s", cartsAtPosition.head.getChar())
          else
            printf("%s", rows(y)(x))
        }
        print("\n")
      }
      print("\n")
    }
  }

  def findFirstCollision(tracksWithCarts: Seq[String]): (Int, Int) =
  {
    val cartsOrNone: Seq[Option[Cart]] = for {
      y <- tracksWithCarts.indices
      x <- 0 until tracksWithCarts.head.length
    }
    yield {
      tracksWithCarts(y)(x) match {
        case '>' => Some(Cart(Vector(x, y), Vector(1, 0), 0))
        case 'v' => Some(Cart(Vector(x, y), Vector(0, 1), 0))
        case '<' => Some(Cart(Vector(x, y), Vector(-1, 0), 0))
        case '^' => Some(Cart(Vector(x, y), Vector(0, -1), 0))
        case _ => None
      }
    }
    val carts = cartsOrNone.flatten

    val track = Track(tracksWithCarts.map(s => s.replace(">", "-").replace("<", "-").replace("^", "|").replace("v", "|")))

    val collision = findFirstCollisionRec(carts, Seq(), track)

    (collision.x, collision.y)
  }

  def findFirstCollisionRec(carts: Seq[Cart], movedCarts: Seq[Cart], track: Track): Vector =
  {
    def detectCollision: Option[Vector] = {
      val positionsMap = (carts ++ movedCarts).map(_.pos).groupBy(identity)
      val collisionPositions = positionsMap.collect { case (pos, instances) if instances.lengthCompare(1) > 0 => pos }
      collisionPositions.headOption
    }

    //track.printWithCarts(carts ++ movedCarts)

    val collisions = detectCollision
    if (collisions.isDefined)
    {
      //track.printWithCarts(carts ++ movedCarts)
      detectCollision.head
    }
    else
    {
      val cart = carts.sortBy(c => c.pos.y -> c.pos.x).head

      if (carts.size == 1)
        findFirstCollisionRec(movedCarts ++ Seq(cart.move(track.get(cart.pos))), Seq(), track)
      else
        findFirstCollisionRec(carts.filterNot(_ == cart), movedCarts ++ Seq(cart.move(track.get(cart.pos))), track)
    }
  }


  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      assert(findFirstCollision(Seq(
        "/->-\\        ",
        "|   |  /----\\",
        "| /-+--+-\\  |",
        "| | |  | v  |",
        "\\-+-/  \\-+--/",
        "  \\------/   ")) == (7,3))

      printf("Tests passed\n")
    }

    // Read input
    //
    val input = Source.fromResource("day13.txt").getLines.toSeq

    print(findFirstCollision(input))
  }

}
