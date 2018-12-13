package day12

import scala.io.Source

object Day12_1
{

  def plantNumberSum(input: Seq[String], generations: Int) : Int =
  {
    val initialState = input.head.split(": ")(1)
    val rules = input.slice(2, input.length).map(s => (s.split(" => ")(0), s.split(" => ")(1))).toMap.withDefaultValue(".")
    plantNumberSumRec(initialState, 0 until initialState.length, 0, generations, rules)
  }

  def plantNumberSumRec(state: String, indices: Seq[Int], generation: Int, generations: Int, rules: Map[String, String]) : Int =
  {
    if (generation == generations)
      state.zip(indices).filter(_._1 == '#').map(_._2).sum
    else
    {
      val paddedState = "...." ++ state ++ "...."
      val nextState = (0 until paddedState.length - 4).map(i => rules(paddedState.substring(i, i + 5))).mkString
      val nextIndices = ((indices.head - 2) until indices.head) ++ indices ++ ((indices.last + 1) until (indices.last + 3))
      assert(nextState.length == nextIndices.length)
      plantNumberSumRec(nextState, nextIndices, generation + 1, generations, rules)
    }
  }

  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      assert(plantNumberSum(Seq("initial state: #..#.#..##......###...###",
        "",
        "...## => #",
        "..#.. => #",
        ".#... => #",
        ".#.#. => #",
        ".#.## => #",
        ".##.. => #",
        ".#### => #",
        "#.#.# => #",
        "#.### => #",
        "##.#. => #",
        "##.## => #",
        "###.. => #",
        "###.# => #",
        "####. => #"), 20) == 325)

      printf("Tests passed\n")
    }

    // Read input
    //
    val input = Source.fromResource("day12.txt").getLines.toSeq

    print(plantNumberSum(input, 20))
  }

}
