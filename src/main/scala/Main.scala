import logic.*
import scala.annotation.tailrec

object Main extends App:
  val rows = 6
  val cols = 6
  
  // 1. SETUP INICIAL
  val initialRng = MyRandom(123L) // A nossa semente determinística
  val initialBoard = Konane.initBoard(rows, cols)
  val initialOpenSpaces = Konane.emptyCoords(initialBoard, rows, cols)
  
  println("--- INÍCIO DA SIMULAÇÃO ALEATÓRIA (KŌNANE) ---")
  println("Tabuleiro Inicial:")
  println(Konane.boardToString(initialBoard, rows, cols))
  
  // 2. O MOTOR DO JOGO (Com a T5 incluída)
  @tailrec
  def gameLoop(
      board: Board, 
      rng: MyRandom, 
      openSpaces: List[Coord2D], 
      currentPlayer: Stone,
      turn: Int
  ): Unit =

    Konane.getWinner(board, currentPlayer, rows, cols) match
      case Some(winner) =>
        println(s"\n❌ FIM DE JOGO no Turno $turn!")
        println(s"As $currentPlayer não têm mais jogadas de captura possíveis.")
        println(s"🏆 AS $winner VENCEM A PARTIDA! 🏆")
        
      case None =>
        println(s"\n--- Turno $turn: Vez das $currentPlayer ---")
        Thread.sleep(1000) 
        
        val (optBoard, nextRng, nextOpenSpaces, optDest) = 
          Konane.playRandomly(board, rng, currentPlayer, openSpaces, Konane.randomMove, rows, cols)
          
        optBoard match
          case Some(newBoard) =>
            println(s">> As $currentPlayer saltaram e aterraram na casa ${optDest.get}")
            println(Konane.boardToString(newBoard, rows, cols))
            
            val nextPlayer = if currentPlayer == Stone.Black then Stone.White else Stone.Black
            gameLoop(newBoard, nextRng, nextOpenSpaces, nextPlayer, turn + 1)
            
          case None =>
            println("Erro inesperado: O getWinner disse que havia jogadas, mas a peça falhou.")

  Thread.sleep(2000)
  gameLoop(initialBoard, initialRng, initialOpenSpaces, Stone.Black, 1)