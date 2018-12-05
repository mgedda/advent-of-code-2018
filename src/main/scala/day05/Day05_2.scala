package day05

import scala.io.Source

object Day05_2
{
  def fullyReduce(polymer: String) : Int =
  {
    val units = "abcdefghijklmnopqrstuvwxyz"
    units.map(c => reactRec(polymer.filterNot(s"$c${c.toUpper}".toSet), Seq()).length).min
  }

  def xor(a: Boolean, b: Boolean) : Boolean = (a || b) && ! (a && b)

  def delete(c1: Char, c2: Char) : Boolean = xor(c1.isUpper, c2.isUpper) && c1.toLower.equals(c2.toLower)

  def shrink(s1: Seq[Char], s2: Seq[Char]) : (Seq[Char], Seq[Char]) =
  {
    if (s1.isEmpty || s2.isEmpty)
      (s1, s2)
    else
    {
      if (delete(s1.last, s2.head))
        shrink(s1.slice(0, s1.length-1), s2.tail)
      else
        (s1, s2)
    }
  }

  def reactRec(polymer: Seq[Char], out: Seq[Char]): String =
  {
    if (polymer.isEmpty)
      out.mkString
    else
    {
      if (polymer.size == 1)
        reactRec(polymer.tail, out ++ polymer)
      else
      {
        if (delete(polymer.head, polymer.tail.head))
        {
          val (out_, polymer_) = shrink(out ++ Seq(polymer.head), polymer.tail)
          reactRec(polymer_, out_)
        }
        else
          reactRec(polymer.tail, out ++ Seq(polymer.head))
      }
    }
  }


  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      assert(fullyReduce("dabAcCaCBAcCcaDA") == 4)
    }

    // Read input
    //
    val polymer = Source.fromResource("day05.txt").getLines.toSeq.head

    println(fullyReduce(polymer))
  }

}
