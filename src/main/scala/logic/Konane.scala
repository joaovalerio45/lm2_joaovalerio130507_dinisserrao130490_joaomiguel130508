package logic

import scala.annotation.tailrec
import scala.collection.parallel.immutable.ParMap

object Konane:

  // Método auxiliar para Inicializar o Tabuleiro
  def initBoard(rows: Int, cols: Int): Board =
    val coords = for
      r <- 0 until rows
      c <- 0 until cols
    yield (r, c)
    
    val map = coords.map { case (r, c) =>
      val stone = if ((r + c) % 2 == 0) Stone.Black else Stone.White
      (r, c) -> stone
    }.toMap
    ParMap.from(map)

  // T1: randomMove
  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) =
    val (idx, nextRand) = rand.nextInt(lstOpenCoords.length)
    (lstOpenCoords(idx), nextRand)

  // T2: play
  def play(board: Board, player: Stone, coordFrom: Coord2D, coordTo: Coord2D, lstOpenCoords: List[Coord2D]): (Option[Board], List[Coord2D]) =
    if !board.get(coordFrom).contains(player) then return (None, lstOpenCoords)
    if board.contains(coordTo) then return (None, lstOpenCoords)

    val (r1, c1) = coordFrom
    val (r2, c2) = coordTo

    val isHorizontal = r1 == r2 && math.abs(c1 - c2) % 2 == 0
    val isVertical = c1 == c2 && math.abs(r1 - r2) % 2 == 0

    if !isHorizontal && !isVertical then return (None, lstOpenCoords)

    val stepR = if r1 == r2 then 0 else if r2 > r1 then 1 else -1
    val stepC = if c1 == c2 then 0 else if c2 > c1 then 1 else -1

    @tailrec
    def checkAndCollectJumped(currR: Int, currC: Int, jumped: List[Coord2D]): Option[List[Coord2D]] =
      if currR == r2 && currC == c2 then Some(jumped)
      else
        val nextR = currR + stepR
        val nextC = currC + stepC
        val jumpedCoord = (nextR, nextC)
        val landCoord = (nextR + stepR, nextC + stepC)
        
        val isOpponent = board.get(jumpedCoord).exists(_ != player)
        val isLandEmpty = !board.contains(landCoord) || landCoord == coordTo

        if isOpponent && isLandEmpty then
          checkAndCollectJumped(landCoord._1, landCoord._2, jumpedCoord :: jumped)
        else None

    checkAndCollectJumped(r1, c1, Nil) match
      case Some(jumpedStones) =>
        val updatedBoard = board - coordFrom -- jumpedStones + (coordTo -> player)
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
    if lstOpenCoords.isEmpty then
      (None, r, lstOpenCoords, None)
    else
      val (coordTo, nextRand) = f(lstOpenCoords, r)
      val playerPieces = board.toList.filter(_._2 == player).map(_._1)
      
      @tailrec
      def tryMoves(pieces: List[Coord2D]): (Option[Board], List[Coord2D], Option[Coord2D]) = pieces match
        case Nil => (None, lstOpenCoords, None)
        case from :: tail =>
          val (optBoard, newOpen) = play(board, player, from, coordTo, lstOpenCoords)
          if optBoard.isDefined then (optBoard, newOpen, Some(coordTo))
          else tryMoves(tail)

      val (finalBoard, finalOpenCoords, moveMade) = tryMoves(playerPieces)
      (finalBoard, nextRand, finalOpenCoords, moveMade)

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
