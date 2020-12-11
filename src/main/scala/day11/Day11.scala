package day11

import aocd.Problem

object Day11 extends Problem(2020, 11) {

  sealed trait GridObject
  case object Empty extends GridObject
  case object Occupied extends GridObject
  case object Floor extends GridObject

  type Grid = Vector[Vector[GridObject]]

  def parse (input: List[String]): Grid = {
    for {
      line <- input.toVector
    } yield for {
      char <- line.toVector
    } yield char match {
      case 'L' => Empty
      case '.' => Floor
    }
  }

  def countAdjacentOccupied (grid: Grid, x: Int, y: Int): Int = {
    def getOption (x: Int, y: Int): Option[GridObject] = 
      grid.lift(y).flatMap(_.lift(x))

    val objectsList = getOption(x - 1, y - 1) :: getOption(x - 1, y) ::
      getOption(x - 1, y + 1) :: getOption(x, y - 1) ::
      getOption(x, y + 1) :: getOption(x + 1, y - 1) ::
      getOption(x + 1, y) :: getOption(x + 1, y + 1) :: Nil

    objectsList.flatten.collect{ case Occupied => Occupied }.length
  }

  def countVisibleOccupied (grid: Grid, x: Int, y: Int): Int = {
    def getVisible (x: Int, y: Int, dx: Int, dy: Int): Option[GridObject] =
      grid.lift(y).flatMap(_.lift(x)).flatMap(_ match {
          case Empty => Some(Empty)
          case Occupied => Some(Occupied)
          case Floor => getVisible(x + dx, y + dy, dx, dy)
        }
      )
    
    val objectsList = getVisible(x - 1, y - 1, -1, -1) :: getVisible(x - 1, y, -1, 0) ::
      getVisible(x - 1, y + 1, -1, 1) :: getVisible(x, y - 1, 0, -1) ::
      getVisible(x, y + 1, 0, 1) :: getVisible(x + 1, y - 1, 1, -1) ::
      getVisible(x + 1, y, 1, 0) :: getVisible(x + 1, y + 1, 1, 1) :: Nil

    objectsList.flatten.collect{ case Occupied => Occupied }.length
  }

  def simulateStep (
    grid: Grid, 
    occupiedCountF: (Grid, Int, Int) => Int,
    tolerance: Int = 4
  ): Grid = {
    for {
      (line, y) <- grid.zipWithIndex
    } yield for {
      (obj, x)  <- line.zipWithIndex
    } yield obj match {
      case Empty => 
        if (occupiedCountF(grid, x, y) == 0) Occupied
        else Empty
      case Occupied => 
        if (occupiedCountF(grid, x, y) >= tolerance) Empty
        else Occupied
      case Floor => Floor
    }
  }

  def findEquilibrium (
    grid: Grid,
    occupiedCountF: (Grid, Int, Int) => Int,
    tolerance: Int = 4
  ): Grid = {
    val newGrid = simulateStep(grid, occupiedCountF, tolerance)

    if (grid == newGrid) grid
    else findEquilibrium(newGrid, occupiedCountF, tolerance)
  }

  def countOccupied (grid: Grid): Int =
    grid.flatten.count(_ == Occupied)
    

  def run(input: List[String]): Unit = {
    val parsed = parse(input)

    println(
      countOccupied(
        findEquilibrium(parsed, countAdjacentOccupied _, 4)
      )
    )

    println(
      countOccupied(
        findEquilibrium(parsed, countVisibleOccupied _, 5)
      )
    )
  }
}
