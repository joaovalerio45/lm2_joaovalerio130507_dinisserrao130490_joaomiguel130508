package logic

import scala.annotation.tailrec
import scala.collection.parallel.immutable.ParMap
import scala.collection.parallel.CollectionConverters.*

object Konane {
    

  // Método auxiliar para Inicializar o Tabuleiro
  def initBoard(rows: Int, cols: Int): Board =
      val coords = List.tabulate(rows, cols)((r, c) => (r, c)).flatten
      
      val board = coords.map { case (r, c) =>
        val stone = if ((r + c) % 2 == 0) Stone.Black else Stone.White
        (r, c) -> stone
      }.toMap.par

      removeInitialPair(board, rows, cols)

  private def removeInitialPair(board: Board, rows: Int, cols: Int): Board =
    val center = (rows / 2 - 1, cols / 2 - 1)
    val adjacent = (center._1, center._2 + 1)
    board - center - adjacent

  def emptyCoords(board: Board, rows: Int, cols: Int): List[Coord2D] = {
  @tailrec
    def loop(r: Int, c: Int, acc: List[Coord2D]): List[Coord2D] =
      if r >= rows then acc
      else if c >= cols then loop(r + 1, 0, acc)
      else
        val newAcc = if !board.contains((r, c)) then (r, c) :: acc else acc
        loop(r, c + 1, newAcc)
        
    loop(0, 0, Nil).reverse
  }
  private val directions: List[Coord2D] = List((1, 0), (-1, 0), (0, 1), (0, -1))

  private def inBounds(coord: Coord2D, rows: Int, cols: Int): Boolean =
    val (r, c) = coord
    r >= 0 && r < rows && c >= 0 && c < cols

  private def nextPosition(coord: Coord2D, direction: Coord2D): Coord2D =
    (coord._1 + direction._1, coord._2 + direction._2)

  private def immediateJumpMoves(current: Coord2D, board: Board, player: Stone, rows: Int, cols: Int): List[(Coord2D, Coord2D)] =
    directions.flatMap { dir =>
      val jumped = nextPosition(current, dir)
      val landing = nextPosition(jumped, dir)
      if board.get(jumped).exists(_ != player) && inBounds(landing, rows, cols) && !board.contains(landing) then
        List((landing, jumped))
      else
        Nil
    }

  private def collectCapturePaths(
      current: Coord2D,
      boardState: Board,
      player: Stone,
      captured: List[Coord2D],
      rows: Int,
      cols: Int
  ): List[(Coord2D, List[Coord2D], Board)] =
    val immediate = immediateJumpMoves(current, boardState, player, rows, cols)
    val currentPaths = if captured.nonEmpty then List((current, captured.reverse, boardState)) else Nil

    val nextPaths = immediate.flatMap { case (landing, jumped) =>
      val movedBoard = boardState - current - jumped + (landing -> player)
      collectCapturePaths(landing, movedBoard, player, jumped :: captured, rows, cols)
    }

    currentPaths ++ nextPaths

  def allCaptureMoves(board: Board, player: Stone, rows: Int, cols: Int): List[(Coord2D, Coord2D, List[Coord2D], Board)] =
    board.collect {
      case (from, stone) if stone == player =>
        collectCapturePaths(from, board, player, Nil, rows, cols).map { case (dest, jumped, finalBoard) => (from, dest, jumped, finalBoard) }
    }.flatten.toList

  private def findCapturePath(board: Board, player: Stone, from: Coord2D, to: Coord2D, rows: Int, cols: Int): Option[(Board, List[Coord2D])] =
    allCaptureMoves(board, player, rows, cols)
      .find { case (start, dest, _, _) => start == from && dest == to }
      .map { case (_, _, jumped, finalBoard) => (finalBoard, jumped) }

  // T1: randomMove
  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) =
    if lstOpenCoords.isEmpty then 
      ((-1, -1), rand)
    else
      val (idx, nextRand) = rand.nextInt
      val index = ((idx % lstOpenCoords.length) + lstOpenCoords.length) % lstOpenCoords.length
      (lstOpenCoords(index), nextRand)

  def smartMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) =
    if lstOpenCoords.isEmpty then
      ((-1, -1), rand)
    else
      // Em vez de ser aleatório, o PC tem uma heurística determinística.
      // Por exemplo: escolhe sempre a casa que está mais perto do fim da lista,
      // o que muda completamente a forma como o jogo se desenrola!
      (lstOpenCoords.last, rand)

  // T2: play
  def play(board: Board, player: Stone, coordFrom: Coord2D, coordTo: Coord2D, lstOpenCoords: List[Coord2D], rows: Int, cols: Int): (Option[Board], List[Coord2D]) =
    if !board.get(coordFrom).contains(player) || board.contains(coordTo) then 
      (None, lstOpenCoords)
    else
      findCapturePath(board, player, coordFrom, coordTo, rows, cols) match
        case Some((updatedBoard, jumpedStones)) =>
          val updatedOpenCoords = (coordFrom :: jumpedStones ::: lstOpenCoords).filter(_ != coordTo)
          (Some(updatedBoard), updatedOpenCoords)
        case None =>
          (None, lstOpenCoords)

// ... (código do T2: play) ...

  // T3: playRandomly
  def playRandomly(
      board: Board,
      r: MyRandom,
      player: Stone,
      lstOpenCoords: List[Coord2D],
      f: (List[Coord2D], MyRandom) => (Coord2D, MyRandom),
      rows: Int,
      cols: Int
  ): (Option[Board], MyRandom, List[Coord2D], Option[Coord2D]) =
    
    val playerPieces = board.toList.filter(_._2 == player).map(_._1).sorted

    @tailrec
    def tryAllOpenSpaces(openSpaces: List[Coord2D], currentRng: MyRandom): (Option[Board], MyRandom, List[Coord2D], Option[Coord2D]) =
      if openSpaces.isEmpty then
        (None, currentRng, lstOpenCoords, None) 
      else
        val (coordTo, nextRng) = f(openSpaces, currentRng)

        @tailrec
        def tryPieces(pieces: List[Coord2D]): (Option[Board], List[Coord2D]) = pieces match
          case Nil => (None, lstOpenCoords)
          case from :: tail =>
            val (optBoard, newOpen) = play(board, player, from, coordTo, lstOpenCoords, rows, cols)
            if optBoard.isDefined then (optBoard, newOpen)
            else tryPieces(tail)

        val (optBoard, finalOpenCoords) = tryPieces(playerPieces)
        
        if optBoard.isDefined then
          (optBoard, nextRng, finalOpenCoords, Some(coordTo))
        else
          val remainingSpaces = openSpaces.filter(_ != coordTo)
          tryAllOpenSpaces(remainingSpaces, nextRng)

    tryAllOpenSpaces(lstOpenCoords, r)

  def boardToString(board: Board, rows: Int, cols: Int): String =
    val header = "  " + (0 until cols).map(c => (c + 'A').toChar).mkString(" ")

    @tailrec
    def buildRows(r: Int, acc: List[String]): List[String] =
      if r >= rows then acc 
      else
        val rowStr = (0 until cols).map { c =>
          board.get((r, c)) match
            case Some(Stone.Black) => "B"
            case Some(Stone.White) => "W"
            case None => "."
        }.mkString(s"$r ", " ", "")
        
        buildRows(r + 1, acc :+ rowStr) 

    val allRows = buildRows(0, Nil).mkString("\n")
    s"$header\n$allRows"

    
    // T5: Verificar Vencedor
  def getWinner(board: Board, currentPlayerTurn: Stone, rows: Int, cols: Int): Option[Stone] =
    // Chamamos a função pura para obter todas as jogadas possíveis deste jogador
    val possibleMoves = allCaptureMoves(board, currentPlayerTurn, rows, cols)
    
    // Se a lista tiver elementos, o jogo continua (devolvemos None)
    if possibleMoves.nonEmpty then 
      None 
    else
      // Se a lista estiver vazia, este jogador perdeu. 
      // Usamos Pattern Matching para descobrir quem é o adversário (o vencedor!)
      currentPlayerTurn match
        case Stone.Black => Some(Stone.White)
        case Stone.White => Some(Stone.Black)
      
  // T6: Função Pura de Undo
  def undo(history: List[GameState]): Option[List[GameState]] =
    // Precisamos de pelo menos 3 estados para anular a nossa jogada e a do computador
    if history.length >= 3 then
      Some(history.drop(2))
    else
      None
}
