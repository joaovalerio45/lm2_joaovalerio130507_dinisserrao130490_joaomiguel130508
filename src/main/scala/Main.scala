import logic.*
import scala.annotation.tailrec
import scala.io.StdIn

object Main extends App:

  val MovePattern = """(\d+)\s+(\d+)\s+(\d+)\s+(\d+)""".r

  val defaultConfig = GameConfig(rows = 6, cols = 6, timeLimitMs = 15000L, difficulty = "Fácil", numPlayers = 1)
  mainMenu(defaultConfig)


  def safeReadLine(): String = {
    val input = StdIn.readLine()
    if (input == null) sys.exit(0) 
    input.trim
  }


  @tailrec
  def mainMenu(config: GameConfig): Unit =
    println("\n" + "="*40)
    println("        KONANE - MENU PRINCIPAL       ")
    println("="*40)
    val modeStr = if config.numPlayers == 1 then "1 Jogador (vs Computador)" else "2 Jogadores (Humano vs Humano)"
    println(s"1. Jogar [$modeStr]")
    println("2. Configurações")
    println("3. Sair")
    print("Escolha uma opção: ")

    safeReadLine() match
      case "1" =>
        val initialRng = MyRandom(12345L)
        val initialBoard = Konane.initBoard(config.rows, config.cols)
        val initialOpenSpaces = Konane.emptyCoords(initialBoard, config.rows, config.cols)
        val initialState = GameState(initialBoard, initialRng, initialOpenSpaces, Stone.Black)

        println("\n>>> A INICIAR O JOGO <<<")
        gameLoop(initialState, Nil, 1, config)
        mainMenu(config)

      case "2" =>
        val newConfig = settingsMenu(config)
        mainMenu(newConfig)

      case "3" =>
        println("A sair do jogo...")

      case _ =>
        println("❌ Opção inválida.")
        mainMenu(config)

  @tailrec
  def settingsMenu(config: GameConfig): GameConfig =
    println("\n--- CONFIGURAÇÕES ---")
    println(s"1. Número de Jogadores: ${config.numPlayers}")
    println(s"2. Dimensões do Tabuleiro: ${config.rows}x${config.cols}")
    println(s"3. Tempo Máximo (segundos): ${config.timeLimitMs / 1000}s")
    println(s"4. Dificuldade do PC: ${config.difficulty}")
    println("5. Voltar ao Menu Principal")
    print("Escolha o que alterar: ")

    safeReadLine() match
      case "1" =>
        val newPlayers = if config.numPlayers == 1 then 2 else 1
        println(s"Modo alterado para $newPlayers Jogador(es).")
        settingsMenu(config.copy(numPlayers = newPlayers))

      case "2" =>
        print("Novas dimensões (ex: 8): ")
        val size = safeReadLine().toIntOption.getOrElse(6)
        settingsMenu(config.copy(rows = size, cols = size))

      case "3" =>
        print("Novo tempo limite em segundos: ")
        val secs = safeReadLine().toLongOption.getOrElse(15L)
        settingsMenu(config.copy(timeLimitMs = secs * 1000L))

      case "4" =>
        val newDiff = if config.difficulty == "Fácil" then "Difícil" else "Fácil"
        println(s"Dificuldade alterada para: $newDiff")
        settingsMenu(config.copy(difficulty = newDiff))

      case "5" =>
        config

      case _ =>
        println("❌ Opção inválida.")
        settingsMenu(config)

  @tailrec
  def gameLoop(
                state: GameState,
                history: List[GameState],
                turn: Int,
                config: GameConfig
              ): Unit =

    Konane.getWinner(state.board, state.currentPlayer, config.rows, config.cols) match
      case Some(winner) if state.midTurnPiece.isEmpty =>
        println(s"\n❌ FIM DE JOGO! 🏆 AS $winner VENCEM A PARTIDA! 🏆")

      case _ =>
        if state.midTurnPiece.isDefined then
          println(s"\n--- Turno $turn: As ${state.currentPlayer} podem continuar a saltar! ---")
          println(s"⚠️ A sua peça aterrou em ${state.midTurnPiece.get}. Pode fazer outro salto com esta mesma peça, ou digitar 'stop' para passar a vez.")
        else
          println(s"\n--- Turno $turn: Vez das ${state.currentPlayer} ---")

        println(Konane.boardToString(state.board, config.rows, config.cols))

        val isComputerTurn = config.numPlayers == 1 && state.currentPlayer == Stone.White

        if !isComputerTurn then
          val commands = if state.midTurnPiece.isDefined then "'r c r c' para continuar ou 'stop' para parar"
          else "'r c r c' | 'undo' | 'restart' | 'quit'"

          print(s"Comando ($commands): ")

          val startTime = System.currentTimeMillis()
          val input = safeReadLine().toLowerCase
          val elapsed = System.currentTimeMillis() - startTime

          if input == "quit" then ()
          else if input == "restart" then
            println("\n🔄 A reiniciar...")
            val initialBoard = Konane.initBoard(config.rows, config.cols)
            val initialOpenSpaces = Konane.emptyCoords(initialBoard, config.rows, config.cols)
            gameLoop(GameState(initialBoard, MyRandom(12345L), initialOpenSpaces, Stone.Black), Nil, 1, config)

          else if elapsed > config.timeLimitMs then
            val adversario = if state.currentPlayer == Stone.Black then Stone.White else Stone.Black
            println(s"⏰ TEMPO ESGOTADO! Demorou ${elapsed / 1000}s.")
            val nextState = state.copy(currentPlayer = adversario, midTurnPiece = None)
            gameLoop(nextState, state :: history, turn + 1, config)

          else
            input match
              case "undo" if state.midTurnPiece.isEmpty =>
                val stepsToUndo = if config.numPlayers == 1 then 2 else 1
                if history.length >= stepsToUndo then
                  val stateToRestore = if stepsToUndo == 2 then history(1) else history.head
                  val newHistory = if stepsToUndo == 2 then history.drop(2) else history.tail
                  println("\n⏪ UNDO ATIVADO!")
                  gameLoop(stateToRestore, newHistory, turn - stepsToUndo, config)
                else
                  println("\n❌ Não há histórico suficiente.")
                  gameLoop(state, history, turn, config)

              case "undo" =>
                println("❌ Não pode fazer undo a meio de um salto múltiplo. Use 'stop' primeiro.")
                gameLoop(state, history, turn, config)

              case "stop" if state.midTurnPiece.isDefined =>
                val nextPlayer = if state.currentPlayer == Stone.Black then Stone.White else Stone.Black
                val nextState = state.copy(currentPlayer = nextPlayer, midTurnPiece = None)
                gameLoop(nextState, state :: history, turn + 1, config)

              case "stop" =>
                println("❌ O comando 'stop' só serve para parar saltos secundários.")
                gameLoop(state, history, turn, config)

              case MovePattern(r1, c1, r2, c2) =>
                val coordFrom = (r1.toInt, c1.toInt)
                val coordTo = (r2.toInt, c2.toInt)

                if state.midTurnPiece.isDefined && state.midTurnPiece.get != coordFrom then
                  println("❌ Erro: Tem de continuar o salto com a mesma peça ou usar 'stop'!")
                  gameLoop(state, history, turn, config)
                else
                  val (optBoard, newOpen) = Konane.play(state.board, state.currentPlayer, coordFrom, coordTo, state.openSpaces, config.rows, config.cols)

                  optBoard match
                    case Some(newBoard) =>
                      val canJumpAgain = Konane.allCaptureMoves(newBoard, state.currentPlayer, config.rows, config.cols).exists(_._1 == coordTo)

                      if canJumpAgain then
                        val nextState = GameState(newBoard, state.rng, newOpen, state.currentPlayer, Some(coordTo))
                        gameLoop(nextState, history, turn, config) 
                      else
                        val nextPlayer = if state.currentPlayer == Stone.Black then Stone.White else Stone.Black
                        val nextState = GameState(newBoard, state.rng, newOpen, nextPlayer, None)
                        gameLoop(nextState, state :: history, turn + 1, config)

                    case None =>
                      println("\n❌ Jogada Inválida. Tente de novo.")
                      gameLoop(state, history, turn, config)

              case _ =>
                println("\n❌ Comando Inválido.")
                gameLoop(state, history, turn, config)

        else
          println("Computador a pensar...")
          val startTime = System.currentTimeMillis()

        
          val (optBoard, nextRng, nextOpenSpaces) = if config.difficulty == "Difícil" then
            val allMoves = Konane.allCaptureMoves(state.board, state.currentPlayer, config.rows, config.cols)
            if allMoves.isEmpty then (None, state.rng, state.openSpaces)
            else
              val bestMove = allMoves.maxBy { case (_, _, jumped, _) => jumped.length }
              val (from, to, jumped, finalBoard) = bestMove
              val updatedOpenCoords = (from :: jumped ::: state.openSpaces).filter(_ != to)
              (Some(finalBoard), state.rng, updatedOpenCoords)
          else
            val (b, r, o, _) = Konane.playRandomly(state.board, state.rng, state.currentPlayer, state.openSpaces, Konane.randomMove, config.rows, config.cols)
            (b, r, o)

          val elapsed = System.currentTimeMillis() - startTime

          if elapsed > config.timeLimitMs then
            println(s"⏰ TEMPO ESGOTADO DO PC! (${elapsed}ms). O PC perdeu o turno.")
            val nextState = state.copy(currentPlayer = Stone.Black, midTurnPiece = None)
            gameLoop(nextState, state :: history, turn + 1, config)
          else
            optBoard match
              case Some(newBoard) =>
                println(s">> O PC efetuou a jogada (em ${elapsed}ms)")
                val nextState = GameState(newBoard, nextRng, nextOpenSpaces, Stone.Black, None)
                gameLoop(nextState, state :: history, turn + 1, config)
              case None =>
                println("Erro: Computador falhou ao jogar ou não tem mais movimentos possíveis.")
