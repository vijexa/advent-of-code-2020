package day1

import aocd.Problem

object Day1 extends Problem(2020, 1) {
  def run(input: List[String]): Unit = println(
    input
      .flatMap(_.toIntOption)
      .combinations(3)
      .find(_.sum == 2020)
      .map(_.product)
  )
}
