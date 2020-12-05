package day5

import aocd.Problem

object Day5 extends Problem(2020, 5) {
  val Row = """.*([BF]{7}).*""".r
  val Col = """.*([LR]{3}).*""".r

  def parseBinary (str: String, lowerChar: Char, upperChar: Char) =
    str.foldLeft((0, math.pow(2, str.length - 1).intValue)){
      // take lower half
      case ((sum, order), `lowerChar`) => (sum, order / 2)
      // take upper half
      case ((sum, order), `upperChar`) => (sum + order, order / 2)
    } match {
      case (sum, _) => sum
    }

  def getRow (str: String): Int =
    str match {
      case Row(row) => parseBinary(row, 'F', 'B')
    }

  def getCol (str: String): Int =
    str match {
      case Col(col) => parseBinary(col, 'L', 'R')
    }

  def getId (str: String): Int = 
    getRow(str) * 8 + getCol(str)


  def run(input: List[String]): Unit = println(
    // task 1
    input
      .map(getId)
      .max,
    // task 2
    input
      .map(getId)
      .sorted
      .sliding(2)
      .collectFirst{
        case x :: xs :: Nil if xs - x != 1 => xs - 1
      }
  )

}


