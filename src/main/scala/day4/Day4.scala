package day4

import aocd.Problem

object Day4 extends Problem(2020, 4) {

  def isValid1(passport: String): Boolean = {
    val regex = (
      """(?=.*ecl)""" +
      """(?=.*pid)""" +
      """(?=.*eyr)""" +
      """(?=.*hcl)""" +
      """(?=.*byr)""" +
      """(?=.*iyr)""" +
      """(?=.*hgt)""" +
      """.*"""
    ).r

    regex.matches(passport)
  }

  def isValid2(passport: String): Boolean = {
    val byr = """^byr:(\d{4})$""".r
    val iyr = """^iyr:(\d{4})$""".r
    val eyr = """^eyr:(\d{4})$""".r
    val hgt = """^hgt:(\d+)(cm|in)$""".r
    val hcl = """^hcl:#[a-f0-9]{6}$""".r
    val ecl = """^ecl:(?:amb|blu|brn|gry|grn|hzl|oth)$""".r
    val pid = """^pid:\d{9}$""".r
    val cid = """^cid:.+$""".r

    def checkValues (list: List[String], verdict: Boolean = true): Boolean = {
      list match {
        case head :: next => 
          val newVerdict = head match {
            case byr(yearStr) => 
              val year = yearStr.toInt
              1920 <= year && year <= 2002

            case iyr(yearStr) => 
              val year = yearStr.toInt
              2010 <= year && year <= 2020

            case eyr(yearStr) => 
              val year = yearStr.toInt
              2020 <= year && year <= 2030

            case hgt(hgtStr, unit) => 
              val height = hgtStr.toInt
              if (unit == "cm") 150 <= height && height <= 193
              else 59 <= height && height <= 76

            case hcl() => true

            case ecl() => true

            case pid() => true

            case cid() => true
            
            case _ => false
          }

          if (newVerdict) checkValues(next, newVerdict)
          else newVerdict
        case Nil => verdict
      }
    }

    if(isValid1(passport)) {
      checkValues(passport.strip.split("""\s""").toList)
    } else false
  }

  def run(input: List[String]): Unit = println(
    input.appended("")
      .foldLeft((0, "")) {
        case ((count, passport), line) =>
          if (line.isBlank) (
            if (isValid2(passport)) count +  1 else count, 
            ""
          )
          else (count, passport + " " + line)
      }
  )
}