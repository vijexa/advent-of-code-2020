package day9

import aocd.Problem
import scala.collection.immutable.Queue

object Day9 extends Problem(2020, 9) {

  def findWrongNumber (input: List[Long]): Option[Long] = {
    def loop (list: List[Long], preamble: Queue[Long]): Option[Long] = 
      list match {
        case head :: next => 
          if (!preamble.combinations(2).exists{ 
            case Queue(a, b) => a + b == head 
            case _ => false
          }) 
            Some(head)
          else 
            loop(next, preamble.drop(1).enqueue(head))
        case Nil => None
      }

    val (preamblePart, inputPart) = input splitAt 25

    loop(inputPart, preamblePart.to(Queue))
  }

  def findWeakness (input: List[Long], target: Long): Option[Long] = {
    def loop (list: List[Long]): Option[Long] =
      list match {
        case head :: next => findSum(list) match {
          case some @ Some(_) => some
          case None => loop(next)
        }
        case Nil => None
      }

    def findSum (
      list: List[Long], 
      sum: Long = 0, 
      min: Long = Long.MaxValue, 
      max: Long = Long.MinValue
    ): Option[Long] =
      list match {
        case head :: next => 
          val currentSum = sum + head
          if (currentSum == target) Some(min + max)
          else if (currentSum > target) None
          else findSum(
            list = next, 
            sum = currentSum, 
            min = if (head < min) head else min,
            max = if (head > max) head else max
          )
        case Nil => None
      }

    loop(input)
  }

  def run (input: List[String]): Unit = {
    val longs = input.map(_.toLong)

    // part 1
    val wrongNumber = findWrongNumber(longs)
    println(wrongNumber)

    // part 2
    println(wrongNumber.flatMap(num => findWeakness(longs, num)))
  }
}
