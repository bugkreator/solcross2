package solcross

class Vector(val row: Int, val column: Int) {
  override def toString() = "(" + row.toString() + "," + column.toString() + ")"

  def +(that: Vector) = Vector(this.row + that.row, this.column + that.column)

  def <(that: Vector) = (this.row < that.row) || ((this.row == that.row) && (this.column < that.column))

  def equals(that: Vector) = (this.row == that.row) && (this.column == that.column)
}

object Vector {
  def apply(row: Int, column: Int) = new Vector(row, column)

  def min(v1: Vector, v2: Vector): Vector = {
    if (v1 < v2) v1 else v2
  }
}


class BoardMove(val From: Vector, val To: Vector) {
  override def toString() = From.toString() + "->" + To.toString()

  def SkippedLocation: Vector = {
    Vector((To.row + From.row) / 2, (To.column + From.column) / 2)
  }

  def this(From: Vector, Direction: Int) = this(From, From + BoardMove.displacementVector(Direction))
}

trait BoardTransformation {
  def apply(v: Vector): Vector

  private def orbitHelper(v: Vector, current: Vector, acc: List[Vector]): List[Vector] = {
    val nextValue: Vector = this(current)
    if (nextValue.equals(v)) acc else orbitHelper(v, nextValue, nextValue :: acc)
  }

  def orbit(v: Vector): List[Vector] = orbitHelper(v, v, List(v))

  def orbitRepresentative(v: Vector): Vector = {
    // just a deterministic way to pick one vector out of every orbit
    orbit(v).fold(v)(Vector.min)
  }

  def isOrbitRepresentative(v: Vector): Boolean = {
    orbitRepresentative(v).equals(v)
  }
}

object HorizontalReflection extends BoardTransformation {
  def apply(v: Vector): Vector = {
    Vector(Board.BoardSize - 1 - v.row, v.column)
  }
}

object VerticalReflection extends BoardTransformation {
  def apply(v: Vector): Vector = {
    Vector(v.row, Board.BoardSize - 1 - v.column)
  }
}

object HalfRotation extends BoardTransformation {
  def apply(v: Vector): Vector = {
    Vector(Board.BoardSize - 1 - v.row, Board.BoardSize - 1 - v.column)
  }
}

object BoardMove {
  val DIR_RIGHT: Int = 0
  val DIR_UP: Int = 1
  val DIR_LEFT: Int = 2
  val DIR_DOWN: Int = 3
  val Directions: List[Int] = List(DIR_RIGHT, DIR_UP, DIR_LEFT, DIR_DOWN)

  def displacementVector(Direction: Int) = Direction match {
    case DIR_RIGHT => Vector(0, 2)
    case DIR_UP => Vector(-2, 0)
    case DIR_LEFT => Vector(0, -2)
    case DIR_DOWN => Vector(2, 0)
  }

  def apply(From: Vector, To: Vector) = new BoardMove(From, To)

  def apply(From: Vector, Direction: Int) = new BoardMove(From, Direction)
}

class Board(val cells: Array[Array[Int]], val MovesSoFar: List[BoardMove]) {

  def isInvariantUnderTransformation(b: BoardTransformation): Boolean = {
    // true iff all v have the same cell value as b(v)
    Board.allPositions.forall((v: Vector) => (this.getCellValue(v) == this.getCellValue(b(v))))
  }

  def getCellValue(Location: Vector): Int = {
    cells(Location.row)(Location.column)
  }

  def getBestMoveList(): List[BoardMove] = {
    val childBoards: List[Board] = getPossibleMoves().map(this.applyMove)
    // TODO: Change this so that if one child returns the best possible list, we don't continue to the next child
    val childLongestLists: List[List[BoardMove]] = childBoards.map((b: Board) => b.getBestMoveList())
    // collect list of best move lists from all children, and return best one. if nothing returns, use current MovesSoFar

    childLongestLists.fold[List[BoardMove]](MovesSoFar)(Board.chooseBetterList)
  }

  def isMovePossible(Move: BoardMove): Boolean = {
    (getCellValue(Move.From) == Board.CELL_FULL) && (getCellValue(Move.To) == Board.CELL_FREE) && (getCellValue(Move.SkippedLocation) == Board.CELL_FULL)
  }

  def getPossibleMoves(): List[BoardMove] = {
    if (MovesSoFar.length >= Board.maxDepth) List()
    else Board.allowableMoves.filter(
      (bm: BoardMove) => this.isMovePossible(bm) && isCellOrbitRepresentativeOfAllSymmetries(bm.From))
  }

  def isCellOrbitRepresentativeOfAllSymmetries(Location: Vector): Boolean = {
    symmetries.forall(_.isOrbitRepresentative(Location))
  }

  def applyMove(Move: BoardMove): Board = {
    Board.incrementCounter()
    val newCells = Array.ofDim[Int](Board.BoardSize, Board.BoardSize)
    // cells.clone() is shallow, need elementwise clone
    for (i <- 0 until Board.BoardSize) {
      newCells(i) = cells(i).clone()
    }

    val skippedLocation: Vector = Move.SkippedLocation
    newCells(skippedLocation.row)(skippedLocation.column) = Board.CELL_FREE
    newCells(Move.From.row)(Move.From.column) = Board.CELL_FREE
    newCells(Move.To.row)(Move.To.column) = Board.CELL_FULL
    new Board(newCells, MovesSoFar :+ Move)
  }

  override def toString() = {
    // TODO: make this not ugly
    val moveToShow: BoardMove = if (MovesSoFar.isEmpty) null else MovesSoFar.last
    val (rowFrom, colFrom, rowTo, colTo): (Int, Int, Int, Int) = if (moveToShow == null) (-1, -1, -1, -1) else (moveToShow.From.row, moveToShow.From.column, moveToShow.To.row, moveToShow.To.column)

    val s: StringBuilder = new StringBuilder()
    s.append(MovesSoFar.toString())
    s.append("\n")
    for (i <- 0 until Board.BoardSize) {
      for (j <- 0 until Board.BoardSize) {
        s.append(if ((i == rowFrom) && (j == colFrom)) "-"
        else if ((i == rowTo) && (j == colTo)) "+"
        else (cells(i)(j) match {
          case Board.CELL_OFF => " "
          case Board.CELL_FREE => "0"
          case Board.CELL_FULL => "1"
        }
          ))
        s.append(" ")
      }
      s.append("\n")
    }
    s.toString()
  }

  // lazy to prevent crashes (otherwise this causes Null pointer exceptions - why?)
  lazy val symmetries = Board.AllBoardTransformations.filter(this.isInvariantUnderTransformation)
}

object Board {
  val AllBoardTransformations: List[BoardTransformation] = List(HalfRotation, VerticalReflection, HorizontalReflection)

  def chooseBetterList(List1: List[BoardMove], List2: List[BoardMove]): List[BoardMove] = {
    if (List2.length > List1.length) List2 else List1
  }

  private var n: Int = 0

  def incrementCounter(): Unit = {
    n += 1
    if (n % 10000000 == 0) println(n)
  }

  val CELL_FREE: Int = 0
  // TODO: Enum?
  val CELL_FULL: Int = 1
  val CELL_OFF: Int = -1
  val initialDefs: Array[Array[Int]] = Array(
    Array(CELL_OFF, CELL_OFF, CELL_FULL, CELL_FULL, CELL_FULL, CELL_OFF, CELL_OFF),
    Array(CELL_OFF, CELL_OFF, CELL_FULL, CELL_FULL, CELL_FULL, CELL_OFF, CELL_OFF),
    Array(CELL_FULL, CELL_FULL, CELL_FULL, CELL_FULL, CELL_FULL, CELL_FULL, CELL_FULL),
    Array(CELL_FULL, CELL_FULL, CELL_FULL, CELL_FREE, CELL_FULL, CELL_FULL, CELL_FULL),
    Array(CELL_FULL, CELL_FULL, CELL_FULL, CELL_FULL, CELL_FULL, CELL_FULL, CELL_FULL),
    Array(CELL_OFF, CELL_OFF, CELL_FULL, CELL_FULL, CELL_FULL, CELL_OFF, CELL_OFF),
    Array(CELL_OFF, CELL_OFF, CELL_FULL, CELL_FULL, CELL_FULL, CELL_OFF, CELL_OFF)
  )

  val BoardSize = initialDefs.size
  val initialBoard: Board = new Board(initialDefs, List())

  def createBoard(): Board = initialBoard

  val maxDepth: Int = 5

  def isLegalLocation(Location: Vector): Boolean = {
    (Location.row >= 0) && (Location.column >= 0) && (Location.row < BoardSize) && (Location.column < BoardSize) && (initialDefs(Location.row)(Location.column) != Board.CELL_OFF)
  }

  val allPositions: List[Vector] = for (i <- List.range(0, Board.BoardSize); j <- List.range(0, Board.BoardSize)) yield Vector(i, j)

  val allowableMoves: List[BoardMove] = {
    val allBoardMoves: List[BoardMove] = for (pos <- allPositions; dir <- BoardMove.Directions) yield BoardMove(pos, dir)
    allBoardMoves.filter((bm: BoardMove) => isLegalLocation(bm.From) && isLegalLocation(bm.To))
  }

  def playMoves(StartingBoard: Board, MovesToPlay: List[BoardMove]): List[Board] = {
    MovesToPlay match {
      case List() => List(StartingBoard)
      case head :: tail => StartingBoard :: playMoves(StartingBoard.applyMove(head), tail)
    }
  }
}
