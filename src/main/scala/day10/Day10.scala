package day10

import aocd.Problem

import util.chaining._
import scala.collection.immutable.Nil

object Day1 extends Problem(2020, 10) {

  def run(input: List[String]): Unit = {
    val parsedInput = input
      .flatMap(_.toIntOption)

    val sortedInput = parsedInput
      .prepended(parsedInput.max + 3) // device
      .sorted

    // part 1
    println(
      sortedInput
        .prepended(0) // outlet
        .sliding(2)
        .foldLeft((0, 0)){
          case ((oneJolts, threeJolts), List(prev, curr)) =>
            if (curr - prev == 1) (oneJolts + 1, threeJolts)
            else if (curr - prev == 3) (oneJolts, threeJolts + 1)
            else (oneJolts, threeJolts)
          case ((oneJolts, threeJolts), _) => (oneJolts, threeJolts)
        } match {
          case (oneJolts, threeJolts) => oneJolts * threeJolts
        }
    )

    // part 2
    println(
      sortedInput
        .foldLeft(Map(0 -> 1L)) {
          case (memo, joltage) => 
            val current = memo.getOrElse(joltage - 1, 0L) + 
              memo.getOrElse(joltage - 2, 0L) +
              memo.getOrElse(joltage - 3, 0L)

            memo + (joltage -> current)
        }
        .values
        .max
    )
  }
}
