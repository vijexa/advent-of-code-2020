package day6

import aocd.Problem

object Day6 extends Problem(2020, 6) {

  def run(input: List[String]): Unit = println(
    // part 1
    input
      .mkString("\n")
      .split("\n\n")
      .foldLeft(0){(sum, group) =>
        sum + group
          .replaceAll("""\s+""", "")
          .toSet
          .size
      },
    // part 2
    input
      .mkString("\n")
      .split("\n\n")
      .foldLeft(0){(sum, group) =>
        val peopleCount = group.split("\n").length
        val grouped = group
          .replaceAll("""\s+""", "")
          .groupBy(identity)
        
        sum + grouped.count { 
          case (_, answers) => answers.length == peopleCount 
        }
      }
  )
}
