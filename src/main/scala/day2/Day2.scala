package day2

import aocd.Problem
import cats.implicits._

object Day2 extends Problem(2020, 2) {

  case class Policy (lower: Int, upper: Int, char: Char)
  case class Password (policy: Policy, password: String)
  
  def parse (str: String): Option[Password] = {
    val splittedStr = str.split(" ")
    for {
      splittedStr <- Option.when(splittedStr.length == 3)(splittedStr)
      Array(bounds, charStr, password) = splittedStr
  
      splittedBounds = bounds.split("-")
      splittedBounds <- Option.when(splittedBounds.length == 2)(splittedBounds)
      Array(fromStr, toStr) = splittedBounds
  
      from <- fromStr.toIntOption
      to   <- toStr.toIntOption
      char <- charStr.headOption
    } yield Password(Policy(from, to, char), password)
  }
  
  def isValid1 (pwd: Password): Boolean = 
    (pwd.policy.lower to pwd.policy.upper) contains pwd.password.count(_ == pwd.policy.char)
  
  def isValid2 (pwd: Password): Boolean = {
    // to get this sweet `get: Option` method
    val pwdVector = pwd.password.toVector
  
    {
      for {
        first <- pwdVector.get(pwd.policy.lower - 1)
        second <- pwdVector.get(pwd.policy.upper - 1)
      } yield first == pwd.policy.char ^ second == pwd.policy.char
    }.getOrElse(false)
  }

  def run(input: List[String]): Unit = println(
    input
      .foldLeft(0) { (validCount, current) =>
        val verdict = for {
          password <- parse(current)
        } yield if (isValid2(password)) validCount + 1 else validCount

        verdict.getOrElse(validCount)
      }
  )
}