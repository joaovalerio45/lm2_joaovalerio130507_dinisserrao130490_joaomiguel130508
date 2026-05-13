import logic.*
import scala.annotation.tailrec

object Main extends App:
  val rows = 6
  val cols = 6
  
  // 1. SETUP INICIAL
  // Usamos o relógio do sistema para gerar jogadas sempre diferentes!
  val initialRng = MyRandom(1234L)
  val initialBoard = Konane.initBoard(rows, cols)
  val initialOpenSpaces = Konane.emptyCoords(initialBoard, rows, cols)
  
  println("--- INÍCIO DA SIMULAÇÃO ALEATÓRIA (KŌNANE) ---")
  println("Tabuleiro Inicial:")
  println(Konane.boardToString(initialBoard, rows, cols))
  
  // 2. O MOTOR DO JOGO (Recursivo)
  @tailrec
  def gameLoop(
      board: Board, 
      rng: MyRandom, 
      openSpaces: List[Coord2D], 
      currentPlayer: Stone,
      turn: Int
  ): Unit =
    println(s"\n--- Turno $turn: Vez das $currentPlayer ---")
    
    // Pequena pausa de 1 segundo para conseguires ver as jogadas a acontecer na consola
    Thread.sleep(1000) 
    
    // Chamamos o teu playRandomly (passando o rows e cols no fim, como corrigimos!)
    val (optBoard, nextRng, nextOpenSpaces, optDest) = 
      Konane.playRandomly(board, rng, currentPlayer, openSpaces, Konane.randomMove, rows, cols)
      
    optBoard match
      case Some(newBoard) =>
        // SUCESSO: A jogada foi feita!
        println(s">> As $currentPlayer saltaram e aterraram na casa ${optDest.get}")
        println(Konane.boardToString(newBoard, rows, cols))
        
        // Determinar de quem é a próxima vez
        val nextPlayer = if currentPlayer == Stone.Black then Stone.White else Stone.Black
        
        // Chamada recursiva para o próximo turno com tudo atualizado
        gameLoop(newBoard, nextRng, nextOpenSpaces, nextPlayer, turn + 1)
        
      case None =>
        // FALHA: Não há mais jogadas válidas. Fim de jogo.
        val winner = if currentPlayer == Stone.Black then Stone.White else Stone.Black
        println(s"\n❌ FIM DE JOGO! As $currentPlayer não têm mais jogadas possíveis.")
        println(s"🏆 AS $winner VENCEM A PARTIDA! 🏆")

  // 3. DAR O TIRO DE PARTIDA
  // Pelas regras, as Pretas (Black) começam sempre primeiro
  Thread.sleep(2000)
  gameLoop(initialBoard, initialRng, initialOpenSpaces, Stone.Black, 1)
  
  
  
  