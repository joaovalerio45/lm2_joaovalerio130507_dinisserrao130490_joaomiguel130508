import logic.*
import scala.annotation.tailrec
import scala.io.StdIn

object Main extends App:

  val MovePattern = """(\d+)\s+(\d+)\s+(\d+)\s+(\d+)""".r

  // Configuração inicial (agora com 1 Jogador por defeito)
  val defaultConfig = GameConfig(rows = 6, cols = 6, timeLimitMs = 15000L, difficulty = "Fácil", numPlayers = 1)
  mainMenu(defaultConfig)

  // ==========================================
  // MENU PRINCIPAL
  // ==========================================
  @tailrec
  def mainMenu(config: GameConfig): Unit =
    println("\n" + "="*40)
    println("        KŌNANE - MENU PRINCIPAL       ")
    println("="*40)
    val modeStr = if config.numPlayers == 1 then "1 Jogador (vs Computador)" else "2 Jogadores (Humano vs Humano)"
    println(s"1. Jogar [$modeStr]")
    println("2. Configurações")
    println("3. Sair")
    print("Escolha uma opção: ")

    StdIn.readLine().trim match
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

  // ==========================================
  // MENU DE CONFIGURAÇÕES
  // ==========================================
  @tailrec
  def settingsMenu(config: GameConfig): GameConfig =
    println("\n--- CONFIGURAÇÕES ---")
    println(s"1. Número de Jogadores: ${config.numPlayers}")
    println(s"2. Dimensões do Tabuleiro: ${config.rows}x${config.cols}")
    println(s"3. Tempo Máximo (segundos): ${config.timeLimitMs / 1000}s")
    println(s"4. Dificuldade do PC: ${config.difficulty}")
    println("5. Voltar ao Menu Principal")
    print("Escolha o que alterar: ")

    StdIn.readLine().trim match
      case "1" =>
        val newPlayers = if config.numPlayers == 1 then 2 else 1
        println(s"Modo alterado para $newPlayers Jogador(es).")
        settingsMenu(config.copy(numPlayers = newPlayers))

      case "2" =>
        print("Novas dimensões (ex: 8): ")
        val size = StdIn.readLine().trim.toIntOption.getOrElse(6)
        settingsMenu(config.copy(rows = size, cols = size))

      case "3" =>
        print("Novo tempo limite em segundos: ")
        val secs = StdIn.readLine().trim.toLongOption.getOrElse(15L)
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

  // ==========================================
  // MOTOR DO JOGO
  // ==========================================
  @tailrec
  def gameLoop(
                state: GameState,
                history: List[GameState],
                turn: Int,
                config: GameConfig
              ): Unit =

    Konane.getWinner(state.board, state.currentPlayer, config.rows, config.cols) match
      // Só declara vitória se o jogador não estiver a meio de uma jogada
      case Some(winner) if state.midTurnPiece.isEmpty =>
        println(s"\n❌ FIM DE JOGO! 🏆 AS $winner VENCEM A PARTIDA! 🏆")

      case _ =>
        // T7: MÚLTIPLOS SALTOS (Continuar ou Parar)
        if state.midTurnPiece.isDefined then
          println(s"\n--- Turno $turn: As ${state.currentPlayer} podem continuar a saltar! ---")
          println(s"⚠️ A sua peça aterrou em ${state.midTurnPiece.get}. Pode fazer outro salto ou parar.")
        else
          println(s"\n--- Turno $turn: Vez das ${state.currentPlayer} ---")

        println(Konane.boardToString(state.board, config.rows, config.cols))

        // A MAGIA DO MULTIPLAYER: Só é turno do computador se for 1 Player E a cor for White.
        val isComputerTurn = config.numPlayers == 1 && state.currentPlayer == Stone.White

        if !isComputerTurn then
          // ---------------------------------------------------------
          // TURNO HUMANO (Serve para as Black, e para as White se for 2 Jogadores)
          // ---------------------------------------------------------
          val commands = if state.midTurnPiece.isDefined then "'r c r c' para continuar ou 'stop' para parar"
          else "'r c r c' | 'undo' | 'restart' | 'quit'"

          print(s"Comando ($commands): ")

          val startTime = System.currentTimeMillis()
          val input = StdIn.readLine().trim.toLowerCase
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
            println(s"🏆 AS $adversario VENCEM POR DESISTÊNCIA! 🏆")

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

              // T7: Ação de PARAR capturas múltiplas
              case "stop" if state.midTurnPiece.isDefined =>
                val nextPlayer = if state.currentPlayer == Stone.Black then Stone.White else Stone.Black
                val nextState = state.copy(currentPlayer = nextPlayer, midTurnPiece = None)
                // Passa a vez ao adversário e guarda no histórico
                gameLoop(nextState, state :: history, turn + 1, config)

              case "stop" =>
                println("❌ O comando 'stop' só serve para parar saltos secundários.")
                gameLoop(state, history, turn, config)

              // T7: Ação de CONTINUAR a jogar (ou jogada normal)
              case MovePattern(r1, c1, r2, c2) =>
                val coordFrom = (r1.toInt, c1.toInt)
                val coordTo = (r2.toInt, c2.toInt)

                // Regra extra: se está a meio de um salto, tem de usar a mesma peça!
                if state.midTurnPiece.isDefined && state.midTurnPiece.get != coordFrom then
                  println("❌ Erro: Tem de continuar o salto com a mesma peça ou usar 'stop'!")
                  gameLoop(state, history, turn, config)
                else
                  val (optBoard, newOpen) = Konane.play(state.board, state.currentPlayer, coordFrom, coordTo, state.openSpaces, config.rows, config.cols)

                  optBoard match
                    case Some(newBoard) =>
                      // Verifica se a peça que acabou de aterrar pode saltar de novo
                      val canJumpAgain = Konane.allCaptureMoves(newBoard, state.currentPlayer, config.rows, config.cols).exists(_._1 == coordTo)

                      if canJumpAgain then
                        // Pode continuar! O currentPlayer mantém-se.
                        val nextState = GameState(newBoard, state.rng, newOpen, state.currentPlayer, Some(coordTo))
                        gameLoop(nextState, history, turn, config) // Não guardamos o histórico a meio do salto
                      else
                        // Não há mais saltos. Passa a vez automaticamente.
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
          // ---------------------------------------------------------
          // TURNO DO COMPUTADOR (Só entra aqui se for 1 Player)
          // ---------------------------------------------------------
          println("Computador a pensar...")
          val startTime = System.currentTimeMillis()

          val aiFunction = if config.difficulty == "Fácil" then Konane.randomMove else Konane.smartMove

          val (optBoard, nextRng, nextOpenSpaces, optDest) =
            Konane.playRandomly(state.board, state.rng, state.currentPlayer, state.openSpaces, aiFunction, config.rows, config.cols)

          val elapsed = System.currentTimeMillis() - startTime

          if elapsed > config.timeLimitMs then
            println(s"⏰ TEMPO ESGOTADO DO PC! (${elapsed}ms). 🏆 AS Black VENCEM! 🏆")
          else
            optBoard match
              case Some(newBoard) =>
                println(s">> O PC saltou para ${optDest.get} (em ${elapsed}ms)")
                // O Computador no nosso código para sempre após 1 salto (estratégia simples)
                val nextState = GameState(newBoard, nextRng, nextOpenSpaces, Stone.Black, None)
                gameLoop(nextState, state :: history, turn + 1, config)
              case None =>
                println("Erro: Computador falhou ao jogar.")