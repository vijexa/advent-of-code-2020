package day7

import aocd.Problem

object Day7 extends Problem(2020, 7) {

  val Reg = """^(\d) (.+) (?:bag|bags)(?:\.)?$""".r

  case class BagRule (color: String, contains: Map[String, Int])

  def containsBag (
    bagToFind: String, 
    bagToCheck: String, 
    bagMap: Map[String, BagRule]
  ): Boolean =
    if (bagToFind == bagToCheck) true
    else bagMap(bagToCheck).contains.exists{ 
      case (color, _) => containsBag(bagToFind, color, bagMap) 
    }

  def bagContains (
    currentBagColor: String,
    bagMap: Map[String, BagRule]
  ): Int = {
    val currentBagRule = bagMap(currentBagColor)

    if (currentBagRule.contains.isEmpty) 1
    else currentBagRule.contains.foldLeft(0){
      case (sum, (color, amount)) => sum + (amount * bagContains(color, bagMap))
    } + 1
  }

  def run(input: List[String]): Unit = {
    val bagMap = input
      .foldLeft(Map.empty[String, BagRule]) { (bagMap, line) =>
        val splitted = line.split(" bags contain ")
        val color = splitted.head
        val contains = splitted.last.split(", ")

        val containsParsed = contains.foldLeft(Map.empty[String, Int]) { 
          (containsMap, str) =>
            str match {
              case Reg(amount, color) => containsMap + (color -> amount.toInt)
              case _ => containsMap
            }
        }

        bagMap + (color -> BagRule(color, containsParsed))
      }

    println(
      bagMap.count{ 
        case (color, bagRule) => containsBag("shiny gold", color, bagMap)
      } - 1
    )

    println(
      bagContains("shiny gold", bagMap) - 1
    )

  }
}