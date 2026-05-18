import logic.*
import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Main extends App:
  val rows = 6
  val cols = 6
  
  println("=====================================")
  println("      KŌNANE - INTERFACE TEXTUAL     ")
  println("=====================================")

  // 1. SETUP INICIAL & T6 (Temporizador Configurável)
  print("⏱️ Defina o tempo máximo por jogada (em segundos): ")
  // Se o utilizador der Enter sem escrever nada ou escrever letras, assume 5 segundos por defeito
  val timeLimitSecs = readLine().toIntOption.getOrElse(5)
  val timeLimitMs = timeLimitSecs * 1000L 
  println(s"✅ Tempo limite configurado para $timeLimitSecs segundos.\n")

  val initialRng = MyRandom(12345L)
  val initialBoard = Konane.initBoard(rows, cols)
  val initialOpenSpaces = Konane.emptyCoords(initialBoard, rows, cols)
  
  // A nossa "fotografia" inicial (usa a Case Class que pusemos no Konane.scala)
  val initialState = GameState(initialBoard, Stone.Black, initialOpenSpaces)
  
  // 2. A MÁQUINA DE ESTADOS (TUI Loop)
  @tailrec
  def tuiLoop(history: List[GameState], rng: MyRandom, turn: Int): Unit =
    val currentState = history.head 
    
    Konane.getWinner(currentState.board, currentState.currentPlayer, rows, cols) match
      case Some(winner) =>
        println(s"\n❌ FIM DE JOGO no Turno $turn!")
        println(Konane.boardToString(currentState.board, rows, cols))
        println(s"🏆 AS $winner VENCEM A PARTIDA! 🏆")
        
      case None =>
        println(s"\n--- Turno $turn: Vez das ${currentState.currentPlayer} ---")
        println(Konane.boardToString(currentState.board, rows, cols))
        
        println("\nEscolha uma ação:")
        println("1 - Fazer Jogada (Computador)")
        println("2 - Undo (Anular última jogada)")
        println("3 - Sair do Jogo")
        print("Opção: ")
        
        readLine() match
          case "1" =>
            // T6: TEMPORIZADOR - Iniciar contagem
            val startTime = System.currentTimeMillis()
            
            val (optBoard, nextRng, nextOpenSpaces, optDest) = 
              Konane.playRandomly(currentState.board, rng, currentState.currentPlayer, currentState.openSpaces, Konane.randomMove, rows, cols)
              
            // T6: TEMPORIZADOR - Parar contagem
            val endTime = System.currentTimeMillis()
            val timeTaken = endTime - startTime
            
            // T6: Validar Limite de Tempo
            if timeTaken > timeLimitMs then
              println(s"\n⚠️ TEMPO ESGOTADO! O cálculo demorou ${timeTaken}ms (limite: ${timeLimitMs}ms).")
              println("A jogada foi anulada. Tente novamente.")
              tuiLoop(history, nextRng, turn) // Repete o turno sem atualizar o histórico
            else
              optBoard match
                case Some(newBoard) =>
                  println(s"\n>> [SUCESSO] Peça saltou para ${optDest.get}. Tempo de execução: ${timeTaken}ms")
                  val nextPlayer = if currentState.currentPlayer == Stone.Black then Stone.White else Stone.Black
                  val newState = GameState(newBoard, nextPlayer, nextOpenSpaces)
                  
                  // Avança para o próximo turno com a nova fotografia no histórico
                  tuiLoop(newState :: history, nextRng, turn + 1)
                  
                case None =>
                  println("Erro inesperado.")
                  tuiLoop(history, nextRng, turn)
                
          case "2" =>
            // T6: UNDO - Chamada à função pura na Camada de Negócio!
            Konane.undo(history) match
              case Some(previousHistory) =>
                println("\n⏪ UNDO ATIVADO: A voltar atrás no tempo...")
                tuiLoop(previousHistory, rng, turn - 2)
              case None =>
                println("\n⚠️ Não há jogadas suficientes no histórico para fazer Undo!")
                tuiLoop(history, rng, turn)
              
          case "3" =>
            println("\nA sair do jogo. Até à próxima!")
            
          case _ =>
            println("\n⚠️ Opção inválida! Escolha 1, 2 ou 3.")
            tuiLoop(history, rng, turn)

  // 3. ARRANCAR O JOGO
  tuiLoop(List(initialState), initialRng, 1)