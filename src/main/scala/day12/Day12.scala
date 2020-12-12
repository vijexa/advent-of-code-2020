package day12

import aocd.Problem
import day12.Day12.North
import day12.Day12.South
import day12.Day12.East
import day12.Day12.West
import day12.Day12.Forward

object Day12 extends Problem(2020, 12) {

  sealed trait Command
  case class North (length: Int) extends Command
  case class South (length: Int) extends Command
  case class East (length: Int) extends Command
  case class West (length: Int) extends Command
  case class Left (angle: Int) extends Command
  case class Right (angle: Int) extends Command
  case class Forward (length: Int) extends Command

  val NorthReg    = """^N(\d+)$""".r
  val SouthReg    = """^S(\d+)$""".r
  val EastReg     = """^E(\d+)$""".r
  val WestReg     = """^W(\d+)$""".r
  val LeftReg     = """^L(\d+)$""".r
  val RightReg    = """^R(\d+)$""".r
  val ForwardReg  = """^F(\d+)$""".r

  def parse (input: List[String]): List[Command] =
    input.map{
      case NorthReg(length)   => North(length.toInt)
      case SouthReg(length)   => South(length.toInt)
      case EastReg(length)    => East(length.toInt)
      case WestReg(length)    => West(length.toInt)
      case LeftReg(angle)     => Left(angle.toInt)
      case RightReg(angle)    => Right(angle.toInt)
      case ForwardReg(length) => Forward(length.toInt)
    }

  case class ShipPosition (northSouth: Int, eastWest: Int, angle: Int) {
    def evaluate (command: Command): ShipPosition = command match {
      case North(length)     => this.copy(northSouth = northSouth + length)
      case South(length)     => this.copy(northSouth = northSouth - length)
      case East(length)      => this.copy(eastWest = eastWest + length)
      case West(length)      => this.copy(eastWest = eastWest - length)
      case Left(deltaAngle)  => changeAngle(-deltaAngle)
      case Right(deltaAngle) => changeAngle(deltaAngle)
      case Forward(length)   => angle match {
        case 0   => evaluate(East(length))
        case 90  => evaluate(South(length))
        case 180 => evaluate(West(length))
        case 270 => evaluate(North(length))
      }
    }

    def changeAngle (deltaAngle: Int): ShipPosition = {
      val newAngle = angle + deltaAngle
      if (newAngle < 0) this.copy(angle = newAngle + 360)
      else if (newAngle > 359) this.copy(angle = newAngle - 360)
      else this.copy(angle = newAngle)
    }
  } 

  case class ShipWithWaypointPosition (
    northSouth: Int, 
    eastWest: Int,
    shipNorthSouth: Int, 
    shipEastWest: Int
  ) {
    def evaluate (command: Command): ShipWithWaypointPosition = command match {
      case North(length)    => this.copy(northSouth = northSouth + length)
      case South(length)    => this.copy(northSouth = northSouth - length)
      case East(length)     => this.copy(eastWest = eastWest + length)
      case West(length)     => this.copy(eastWest = eastWest - length)
      case Left(angle)      => changeAngle(-angle)
      case Right(angle)     => changeAngle(angle)
      case Forward(length)  => this.copy(
        shipNorthSouth = shipNorthSouth + northSouth * length,
        shipEastWest = shipEastWest + eastWest * length
      )
    }

    def changeAngle (deltaAngle: Int): ShipWithWaypointPosition = {
      val angle = deltaAngle.toRadians

      import math._
      def rotatePoint (x: Int, y: Int): (Int, Int) = (
        (x * cos(angle) - y * sin(angle)).round.toInt,
        (y * cos(angle) + x * sin(angle)).round.toInt
      )

      rotatePoint(northSouth, eastWest) match {
        case (x, y) => this.copy(northSouth = x, eastWest = y)
      }
    }
  }

  def evaluateCommandsP1 (input: List[Command]): ShipPosition =
    input.foldLeft(ShipPosition(0, 0, 0))((position, command) =>
      position evaluate command
    )

  def evaluateCommandsP2 (input: List[Command]): ShipWithWaypointPosition =
    input.foldLeft(ShipWithWaypointPosition(1, 10, 0, 0))((position, command) =>
      position evaluate command
    )

  def run(input: List[String]): Unit = {
    
    val parsed = parse(input)

    val evaluated1 = evaluateCommandsP1(parsed)

    println(evaluated1)

    println(math.abs(evaluated1.eastWest) + math.abs(evaluated1.northSouth))

    val evaluated2 = evaluateCommandsP2(parsed)

    println(math.abs(evaluated2.shipEastWest) + math.abs(evaluated2.shipNorthSouth))
  }
}
