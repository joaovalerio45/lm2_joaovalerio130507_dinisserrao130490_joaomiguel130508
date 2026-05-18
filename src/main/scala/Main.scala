import logic.*
import scala.annotation.tailrec

object Main extends App:
  val rows = 6
  val cols = 6

  val initialRng = MyRandom(1234L)
  val initialBoard = Konane.initBoard(rows, cols)
  val initialOpenSpaces = Konane.emptyCoords(initialBoard, rows, cols)
  
  println("Tabuleiro Inicial:")
  println(Konane.boardToString(initialBoard, rows, cols))
  
  @tailrec
  def gameLoop(
      board: Board, 
      rng: MyRandom, 
      openSpaces: List[Coord2D], 
      currentPlayer: Stone,
      turn: Int
  ): Unit =
    println(s"\n--- Turno $turn: Vez das $currentPlayer ---")
    
    Thread.sleep(1000) 
    
    // placeholder ( random plays consecutivas)
    val (optBoard, nextRng, nextOpenSpaces, optDest) = 
      Konane.playRandomly(board, rng, currentPlayer, openSpaces, Konane.randomMove, rows, cols)
      
    optBoard match
      case Some(newBoard) =>
        println(s">> As $currentPlayer saltaram e aterraram na casa ${optDest.get}")
        println(Konane.boardToString(newBoard, rows, cols))
        
        val nextPlayer = if currentPlayer == Stone.Black then Stone.White else Stone.Black
        
        gameLoop(newBoard, nextRng, nextOpenSpaces, nextPlayer, turn + 1)
        
      case None =>
        val winner = if currentPlayer == Stone.Black then Stone.White else Stone.Black
        println(s"\n❌ FIM DE JOGO! As $currentPlayer não têm mais jogadas possíveis.")
        println(s"🏆 AS $winner VENCEM A PARTIDA! 🏆")

 
  Thread.sleep(2000)
  gameLoop(initialBoard, initialRng, initialOpenSpaces, Stone.Black, 1)
  
  
  
  