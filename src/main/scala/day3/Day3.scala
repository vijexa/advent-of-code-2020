package day3

import aocd.Problem

object Day3 extends Problem(2020, 3) {

  def countTrees(
    map: Seq[String],
    slopeX: Int,
    slopeY: Int,
    treeCount: Long = 0, 
    x: Int = 0, 
    y: Int = 0
  ): Long = 
    if (y < map.length) {
      val strLen = map(y).length
      val boundedX = if (x >= strLen) x - strLen else x

      val newTreeCount = map(y)(boundedX) match {
        case '.' => treeCount
        case '#' => treeCount + 1
      }

      countTrees(map, slopeX, slopeY, newTreeCount, boundedX + slopeX, y + slopeY)
    } else treeCount

  def run(input: List[String]): Unit = println(
    countTrees(input, 1, 1) *
    countTrees(input, 3, 1) *
    countTrees(input, 5, 1) *
    countTrees(input, 7, 1) *
    countTrees(input, 1, 2)
  )
}