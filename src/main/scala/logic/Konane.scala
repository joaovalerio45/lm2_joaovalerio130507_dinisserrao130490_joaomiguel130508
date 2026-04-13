package logic

import scala.annotation.tailrec
import scala.collection.parallel.immutable.ParMap
import scala.collection.parallel.CollectionConverters.*

object Konane:

  // Método auxiliar para Inicializar o Tabuleiro
  def initBoard(rows: Int, cols: Int): Board =
    val coords = for
      r <- 0 until rows
      c <- 0 until cols
    yield (r, c)
    
    val board = coords.map { case (r, c) =>
      val stone = if ((r + c) % 2 == 0) Stone.Black else Stone.White
      (r, c) -> stone
    }.toMap.par

    removeInitialPair(board, rows, cols)

  private def removeInitialPair(board: Board, rows: Int, cols: Int): Board =
    val center = (rows / 2 - 1, cols / 2 - 1)
    val adjacent = (center._1, center._2 + 1)
    board - center - adjacent

  def emptyCoords(board: Board, rows: Int, cols: Int): List[Coord2D] =
    (for
      r <- 0 until rows
      c <- 0 until cols
      coord = (r, c)
      if !board.contains(coord)
    yield coord).toList

  private val directions: List[Coord2D] = List((1, 0), (-1, 0), (0, 1), (0, -1))

  private def inBounds(coord: Coord2D, board: Board): Boolean =
    val rows = board.keys.map(_._1)
    val cols = board.keys.map(_._2)
    val (r, c) = coord
    r >= rows.min && r <= rows.max && c >= cols.min && c <= cols.max

  private def nextPosition(coord: Coord2D, direction: Coord2D): Coord2D =
    (coord._1 + direction._1, coord._2 + direction._2)

  private def immediateJumpMoves(current: Coord2D, board: Board, player: Stone): List[(Coord2D, Coord2D)] =
    directions.flatMap { dir =>
      val jumped = nextPosition(current, dir)
      val landing = nextPosition(jumped, dir)
      if board.get(jumped).exists(_ != player) && inBounds(landing, board) && !board.contains(landing) then
        List((landing, jumped))
      else
        Nil
    }

  private def collectCapturePaths(
      current: Coord2D,
      boardState: Board,
      player: Stone,
      captured: List[Coord2D]
  ): List[(Coord2D, List[Coord2D], Board)] =
    val immediate = immediateJumpMoves(current, boardState, player)
    val currentPaths = if captured.nonEmpty then List((current, captured.reverse, boardState)) else Nil

    val nextPaths = immediate.flatMap { case (landing, jumped) =>
      val movedBoard = boardState - current - jumped + (landing -> player)
      collectCapturePaths(landing, movedBoard, player, jumped :: captured)
    }

    currentPaths ++ nextPaths

  def allCaptureMoves(board: Board, player: Stone): List[(Coord2D, Coord2D, List[Coord2D], Board)] =
    board.toList.collect {
      case (from, stone) if stone == player =>
        collectCapturePaths(from, board, player, Nil).map { case (dest, jumped, finalBoard) => (from, dest, jumped, finalBoard) }
    }.flatten

  private def findCapturePath(board: Board, player: Stone, from: Coord2D, to: Coord2D): Option[(Board, List[Coord2D])] =
    allCaptureMoves(board, player)
      .find { case (start, dest, _, _) => start == from && dest == to }
      .map { case (_, _, jumped, finalBoard) => (finalBoard, jumped) }

  // T1: randomMove
  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) =
    val (idx, nextRand) = rand.nextInt
    val index = if lstOpenCoords.isEmpty then 0 else ((idx % lstOpenCoords.length) + lstOpenCoords.length) % lstOpenCoords.length
    (lstOpenCoords(index), nextRand)

  // T2: play
  def play(board: Board, player: Stone, coordFrom: Coord2D, coordTo: Coord2D, lstOpenCoords: List[Coord2D]): (Option[Board], List[Coord2D]) =
    if !board.get(coordFrom).contains(player) then return (None, lstOpenCoords)
    if board.contains(coordTo) then return (None, lstOpenCoords)

    findCapturePath(board, player, coordFrom, coordTo) match
      case Some((updatedBoard, jumpedStones)) =>
        val updatedOpenCoords = (coordFrom :: jumpedStones ::: lstOpenCoords).filter(_ != coordTo)
        (Some(updatedBoard), updatedOpenCoords)
      case None =>
        (None, lstOpenCoords)

  // T3: playRandomly
  def playRandomly(
      board: Board,
      r: MyRandom,
      player: Stone,
      lstOpenCoords: List[Coord2D],
      f: (List[Coord2D], MyRandom) => (Coord2D, MyRandom)
  ): (Option[Board], MyRandom, List[Coord2D], Option[Coord2D]) =
    val (coordTo, nextRand) = f(lstOpenCoords, r)
    val playerPieces = board.toList.filter(_._2 == player).map(_._1)
    
    @tailrec
    def tryMoves(pieces: List[Coord2D]): (Option[Board], List[Coord2D]) = pieces match
      case Nil => (None, lstOpenCoords)
      case from :: tail =>
        val (optBoard, newOpen) = play(board, player, from, coordTo, lstOpenCoords)
        if optBoard.isDefined then (optBoard, newOpen)
        else tryMoves(tail)

    val (finalBoard, finalOpenCoords) = tryMoves(playerPieces)
    finalBoard match
      case Some(boardAfterMove) => (Some(boardAfterMove), nextRand, finalOpenCoords, Some(coordTo))
      case None => (None, nextRand, lstOpenCoords, None)

  // T4: printBoard
  def printBoard(board: Board, rows: Int, cols: Int): Unit =
    val header = "  " + (0 until cols).map(c => (c + 'A').toChar).mkString(" ")
    println(header)
    for r <- 0 until rows do
      print(s"$r ")
      for c <- 0 until cols do
        board.get((r, c)) match
          case Some(Stone.Black) => print("B ")
          case Some(Stone.White) => print("W ")
          case None => print(". ")
      println()
