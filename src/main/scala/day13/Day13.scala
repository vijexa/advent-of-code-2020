package day13

import aocd.Problem

object Day13 extends Problem(2020, 13) {
  def run(input: List[String]): Unit = {
    // part 1
    val (earliestTimestamp, busIntervals) = input match {
      case head :: next :: Nil => (
        head.toInt, 
        next.split(",").flatMap(_.toIntOption)
      )
    }

    val bestBus = busIntervals.map(id => 
      (id -> (id - (earliestTimestamp % id)))
    ).minBy{ case (_, timeWaiting) => timeWaiting }

    println(
      bestBus match {
        case (id, timeWaiting) => id * timeWaiting
      }
    )

    // part 2
    case class Bus (id: Int, order: Int)
    val buses = input(1).split(",").zipWithIndex.flatMap{
      case (busIdStr, order) => busIdStr.toIntOption match {
        case Some(busId) => Some(Bus(busId, order))
        case None => None
      }
    }

    println(
      buses.foldLeft((0L, 1L)){
        case ((t, interval), Bus(busId, i)) => 
          def findTime (time: Long): Long = 
            if ((time + i) % busId == 0) time
            else findTime(time + interval)
          
          (findTime(t), interval * busId)
      } match { case (t, _) => t }
    )
  }
}
