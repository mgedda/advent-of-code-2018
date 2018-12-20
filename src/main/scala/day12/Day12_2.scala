package day12

import scala.io.Source

object Day12_2
{

  def plantNumberSum(input: Seq[String], generations: Long) : Long =
  {
    val initialState = input.head.split(": ")(1)
    val rules = input.slice(2, input.length).map(s => (s.split(" => ")(0), s.split(" => ")(1))).toMap.withDefaultValue(".")

    plantNumberSumRec(initialState.zip(0 until initialState.length), 0, generations, Map(), rules)
  }

  def getNextStateAndIndices(stateWithIndex: IndexedSeq[(Char, Int)], rules: Map[String, String]): IndexedSeq[(Char, Int)] =
  {
    val state = stateWithIndex.map(_._1).mkString
    val indices = stateWithIndex.map(_._2)

    val paddedState = "...." ++ state ++ "...."
    val nextState = (0 until paddedState.length - 4).par.map(i => rules(paddedState.substring(i, i + 5))).mkString
    val nextIndices = ((indices.head - 2) until indices.head) ++ indices ++ ((indices.last + 1) until (indices.last + 3))
    assert(nextState.length == nextIndices.length)

    val first = nextState.indexOf('#')
    val last = nextState.lastIndexOf('#')
    val nextStateTrimmed = nextState.substring(first, last + 1)
    val nextIndicesTrimmed: IndexedSeq[Int] = nextIndices.slice(first, last + 1)
    assert(nextStateTrimmed.length == nextIndicesTrimmed.length)

    nextStateTrimmed.zip(nextIndicesTrimmed)
  }

  def plantNumberSumRec(stateWithIndex: IndexedSeq[(Char, Int)],
                        count: Long,
                        generations: Long,
                        visitedStates: Map[IndexedSeq[(Char, Int)], Long],
                        rules: Map[String, String]) : Long =
  {
    if (count == 10000)
      stateWithIndex.filter(_._1 == '#').map(_._2 + generations - count).sum
    else
    {
      val nextStateWithIndex = getNextStateAndIndices(stateWithIndex, rules)
      plantNumberSumRec(nextStateWithIndex, count + 1, generations, visitedStates + (stateWithIndex -> count), rules)
    }
  }

  def main(args: Array[String]): Unit =
  {
    // Read input
    //
    val input = Source.fromResource("day12.txt").getLines.toSeq

    print(plantNumberSum(input, 50000000000l))
  }

}
