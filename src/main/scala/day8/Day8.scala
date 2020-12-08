package day8

import aocd.Problem

object Day8 extends Problem(2020, 8) {

  sealed trait Command

  object Command {
    case class Acc (increment: Int) extends Command
    case class Jmp (destination: Int) extends Command
    case class Nop (value: Int) extends Command
  }

  import Command._

  val CommandReg = """^(acc|nop|jmp) ((?:\-|\+)\d+)$""".r

  def parseLine (str: String): Option[Command] = 
    str match {
      case CommandReg(command, value) => command match {
        case "acc" => Some(Acc(value.toInt))
        case "nop" => Some(Nop(value.toInt))
        case "jmp" => Some(Jmp(value.toInt))
        case _     => None
      }
      case _ => None
    }

  def interpret (
    program: Vector[Command], 
    i: Int = 0,
    acc: Int = 0, 
    executed: Set[Int] = Set.empty
  ): Either[Int, Int] = {
    if (executed contains i) {
      Left(acc)
    } else if (i == program.length) {
      Right(acc)
    } else {
      val command = program(i)
      command match {
        case Acc(increment) => 
          interpret(program, i + 1, acc + increment, executed + i)
        case Jmp(destination) =>
          interpret(program, i + destination, acc, executed + i)
        case Nop(_) =>
          interpret(program, i + 1, acc, executed + i)
      }
    }
  }

  def fixProgram (program: Vector[Command]): Option[Vector[Command]] = {
    def loop (tryingToFix: Int): Option[Vector[Command]] = {
      if (tryingToFix > 0) {
        val command = program(tryingToFix)
        val newCommand = command match {
          case Jmp(destination) => Nop(destination)
          case Nop(value) => Jmp(value)
          case c => c
        }

        if (newCommand != command) {
          val newProgram = program.updated(tryingToFix, newCommand)
          
          interpret(newProgram) match {
            case Left(output) => loop(tryingToFix - 1)
            case Right(output) => Some(newProgram)
          }
        } else (
          loop(tryingToFix - 1)
        )
      } else {
        None
      }
    }

    loop(program.length - 1)
  }
  

  def run(input: List[String]): Unit = {
    val parsed = input
      .flatMap(parseLine)
      .toVector
  
    println(interpret(parsed))

    println(interpret(fixProgram(parsed).get))
    
  }
}
