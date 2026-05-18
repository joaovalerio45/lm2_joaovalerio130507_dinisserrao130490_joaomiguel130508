package gui

import javafx.application.Application
import javafx.scene.Scene
import javafx.scene.layout.{BorderPane, GridPane, HBox, VBox}
import javafx.scene.control.{Button, Label, ComboBox, TextField, Alert}
import javafx.scene.control.Alert.AlertType
import javafx.scene.shape.Circle
import javafx.scene.paint.Color
import javafx.stage.Stage
import javafx.geometry.Insets
import javafx.geometry.Pos
import javafx.animation.{KeyFrame, Timeline, PauseTransition}
import javafx.util.Duration
import javafx.animation.Animation
import scala.annotation.tailrec
import scala.compiletime.uninitialized

import logic.*

class KonaneInterface extends Application {
  
  val rows = 6
  val cols = 6
  
  var currentBoard: Board = Konane.initBoard(rows, cols)
  var openSpaces: List[Coord2D] = Konane.emptyCoords(currentBoard, rows, cols)
  var currentPlayer: Stone = Stone.Black
  var selectedCoord: Option[Coord2D] = None
  

  var history: List[(Board, List[Coord2D], Stone)] = Nil

  var isVsBot: Boolean = false
  var rng: MyRandom = MyRandom(1234L)
  var maxTimePerMove: Int = 30
  var timeRemaining: Int = 30
  var botDifficulty: String = "Fácil"


  val boardGrid = new GridPane()
  val turnLabel = new Label("Turno: Pretas")
  val timerLabel = new Label("Tempo: 30s")
  var gameTimeline: Timeline = uninitialized
  var mainStage: Stage = uninitialized

  override def start(primaryStage: Stage): Unit = {
    mainStage = primaryStage
    showMainMenu()
  }


  def showMainMenu(): Unit = {
    stopTimer()
    val menuLayout = new VBox(15)
    menuLayout.setPadding(new Insets(25))
    menuLayout.setAlignment(Pos.CENTER)
    menuLayout.setStyle("-fx-background-color: #f4f4f4;")

    val titleLabel = new Label("KŌNANE - CONFIGURAÇÃO JOGO (6x6)")
    titleLabel.setStyle("-fx-font-weight: bold; -fx-font-size: 16px; -fx-text-fill: #333;")

    val hbMode = new HBox(10)
    hbMode.setAlignment(Pos.CENTER)
    val cbMode = new ComboBox[String]()
    cbMode.getItems.addAll("Jogador vs Jogador", "Jogador vs Computador")
    cbMode.setValue("Jogador vs Jogador")
    hbMode.getChildren.addAll(new Label("Modo de Jogo:"), cbMode)

    val hbDiff = new HBox(10)
    hbDiff.setAlignment(Pos.CENTER)
    val cbDiff = new ComboBox[String]()
    cbDiff.getItems.addAll("Fácil", "Difícil")
    cbDiff.setValue("Fácil")
    hbDiff.getChildren.addAll(new Label("Dificuldade do Bot:"), cbDiff)

    val hbTimer = new HBox(10)
    hbTimer.setAlignment(Pos.CENTER)
    val txtTimer = new TextField("30")
    txtTimer.setPrefWidth(60)
    hbTimer.getChildren.addAll(new Label("Tempo Limite por Jogada (segundos):"), txtTimer)

    val btnStart = new Button("Iniciar Partida")
    btnStart.setMinSize(140, 40)
    btnStart.setStyle("-fx-base: #2ecc71; -fx-font-weight: bold;")
    
    btnStart.setOnAction(_ => {
      isVsBot = cbMode.getValue == "Jogador vs Computador"
      botDifficulty = cbDiff.getValue
      maxTimePerMove = txtTimer.getText.toIntOption.getOrElse(30).max(5).min(300)
      
      resetGameEngine()
      showGameView()
    })

    menuLayout.getChildren.addAll(titleLabel, hbMode, hbDiff, hbTimer, btnStart)

    val scene = new Scene(menuLayout, 460, 280)
    mainStage.setTitle("Kōnane - Menu Principal")
    mainStage.setScene(scene)
    mainStage.show()
  }

  // ==========================================
  // VISTA DO JOGO (Scene Graph)
  // ==========================================
  def showGameView(): Unit = {
    val root = new BorderPane()
    root.setPadding(new Insets(15))

    // TOP: Botões de Ação de Fluxo e Controlos (T6/T7)
    val topControls = new HBox(12)
    topControls.setPadding(new Insets(0, 0, 15, 0))
    topControls.setAlignment(Pos.CENTER_LEFT)
    
    val btnRestart = new Button("Reiniciar")
    val btnUndo = new Button("Undo (Desfazer)")
    val btnMenu = new Button("Configurações")
    
    topControls.getChildren.addAll(btnMenu, btnRestart, btnUndo)
    root.setTop(topControls)

    // CENTER: Tabuleiro centralizado
    boardGrid.setHgap(6)
    boardGrid.setVgap(6)
    boardGrid.setAlignment(Pos.CENTER)
    updateBoardUI()
    root.setCenter(boardGrid)

    // BOTTOM: Barra de Estado com Turno e Contador Regressivo (T5/T6)
    val statusBar = new HBox(40)
    statusBar.setPadding(new Insets(15, 0, 0, 0))
    statusBar.setAlignment(Pos.CENTER_LEFT)
    
    turnLabel.setStyle("-fx-font-weight: bold; -fx-font-size: 13px;")
    timerLabel.setStyle("-fx-font-weight: bold; -fx-font-size: 13px; -fx-text-fill: #e74c3c;")
    
    statusBar.getChildren.addAll(turnLabel, timerLabel)
    root.setBottom(statusBar)

    // Eventos dos Botões de Fluxo
    btnRestart.setOnAction(_ => {
      resetGameEngine()
      updateBoardUI()
    })

    btnMenu.setOnAction(_ => {
      showMainMenu()
    })

    // T6: Lógica Funcional do Botão de Undo
    btnUndo.setOnAction(_ => {
      history match {
        case Nil => 
          println("Nenhuma jogada registada no histórico.")
        case (prevBoard, prevOpen, prevPlayer) :: tail =>
          // Se jogarmos contra Bot, o Undo deve reverter a jogada do Bot E a do Humano
          if (isVsBot && tail.nonEmpty) {
            val (userBoard, userOpen, userPlayer) :: userTail = tail
            currentBoard = userBoard
            openSpaces = userOpen
            currentPlayer = userPlayer
            history = userTail
          } else {
            currentBoard = prevBoard
            openSpaces = prevOpen
            currentPlayer = prevPlayer
            history = tail
          }
          selectedCoord = None
          resetTimer()
          turnLabel.setText(s"Turno: $currentPlayer")
          updateBoardUI()
          println("Undo efetuado com sucesso.")
      }
    })

    val scene = new Scene(root, 450, 500)
    mainStage.setTitle("Kōnane - Tabuleiro 6x6")
    mainStage.setScene(scene)
    
    startTimer()
  }

  def resetGameEngine(): Unit = {
    currentBoard = Konane.initBoard(rows, cols)
    openSpaces = Konane.emptyCoords(currentBoard, rows, cols)
    currentPlayer = Stone.Black
    selectedCoord = None
    history = Nil
    resetTimer()
    turnLabel.setText(s"Turno: $currentPlayer")
  }

  // ==========================================
  // T6: SISTEMA DO TEMPORIZADOR (TIMELINE)
  // ==========================================
  def startTimer(): Unit = {
    stopTimer()
    timeRemaining = maxTimePerMove
    timerLabel.setText(s"Tempo: ${timeRemaining}s")
    
    gameTimeline = new Timeline(new KeyFrame(Duration.seconds(1), _ => {
      timeRemaining -= 1
      timerLabel.setText(s"Tempo: ${timeRemaining}s")
      
      if (timeRemaining <= 0) {
        stopTimer()
        // O jogador atual perde por falta de tempo; o oponente vence (T5)
        val winner = if (currentPlayer == Stone.Black) Stone.White else Stone.Black
        turnLabel.setText(s"TIMEOUT! Vitória: $winner")
        showAlert("Fim de Jogo", s"O tempo limite de ${maxTimePerMove}s expirou! Vitória das peças ${winner}.")
      }
    }))
    gameTimeline.setCycleCount(Animation.INDEFINITE)
    gameTimeline.play()
  }

  def stopTimer(): Unit = {
    if (gameTimeline != null) gameTimeline.stop()
  }

  def resetTimer(): Unit = {
    timeRemaining = maxTimePerMove
    if (gameTimeline != null) {
      gameTimeline.playFromStart()
    }
  }

  // ==========================================
  // LÓGICA DE EVENTOS E CAPTURA DE CLIQUES (T8)
  // ==========================================
  def handleCellClick(r: Int, c: Int): Unit = {
    if (isVsBot && currentPlayer == Stone.White) return 

    val clickedCoord = (r, c)

    selectedCoord match {
      case None =>
        currentBoard.get(clickedCoord) match {
          case Some(stone) if stone == currentPlayer =>
            selectedCoord = Some(clickedCoord)
            updateBoardUI()
          case _ => 
            println("Selecione uma peça da sua cor.")
        }

      case Some(fromCoord) =>
        if (fromCoord == clickedCoord) {
          selectedCoord = None
          updateBoardUI()
        } else {
          val previousState = (currentBoard, openSpaces, currentPlayer)
          val (optBoard, newOpenSpaces) = Konane.play(currentBoard, currentPlayer, fromCoord, clickedCoord, openSpaces, rows, cols)

          optBoard match {
            case Some(newBoard) =>
              history = previousState :: history
              
              // VERIFICAÇÃO DE CAPTURA MÚLTIPLA:
              // Verifica se a peça que acabou de aterrar em 'clickedCoord' ainda tem movimentos de captura possíveis no 'newBoard'
              val remainingMovesForPiece = Konane.allCaptureMoves(newBoard, currentPlayer, rows, cols)
                .filter { case (start, _, _, _) => start == clickedCoord }

              if (remainingMovesForPiece.nonEmpty) {
                // Atualiza o tabuleiro temporariamente para o jogador ver onde a peça aterrou
                currentBoard = newBoard
                openSpaces = newOpenSpaces
                updateBoardUI()

                // Abre a caixa de diálogo perguntando se quer continuar a capturar (T7/T8)
                if (askToContinueCapturing()) {
                  // SIM: Mantém o turno no mesmo jogador e força a seleção na nova coordenada da peça
                  selectedCoord = Some(clickedCoord)
                  resetTimer()
                  updateBoardUI()
                  println(s"O jogador decidiu continuar a capturar com a peça em $clickedCoord")
                } else {
                  // NÃO: Finaliza a jogada e passa o turno normalmente
                  executeGameStateTransition(newBoard, newOpenSpaces)
                }
              } else {
                // Sem capturas adicionais disponíveis: avança o turno diretamente
                executeGameStateTransition(newBoard, newOpenSpaces)
              }

            case None =>
              currentBoard.get(clickedCoord) match {
                case Some(stone) if stone == currentPlayer =>
                  selectedCoord = Some(clickedCoord)
                  updateBoardUI()
                case _ =>
                  println("Movimento inválido!")
              }
          }
        }
    }
  }

  private def askToContinueCapturing(): Boolean = {
    import javafx.scene.control.ButtonType
    
    val alert = new Alert(AlertType.CONFIRMATION)
    alert.setTitle("Captura Múltipla Disponível")
    alert.setContentText("Desejas continuar a jogar para realizar outra captura?")
    
    // Customizar botões para Sim e Não
    val btnSim = new ButtonType("Sim")
    val btnNao = new ButtonType("Não")
    alert.getButtonTypes.setAll(btnSim, btnNao)
    
    val result = alert.showAndWait()
    result.isPresent && result.get() == btnSim
  }

  def executeGameStateTransition(newBoard: Board, newOpenSpaces: List[Coord2D]): Unit = {
    currentBoard = newBoard
    openSpaces = newOpenSpaces
    currentPlayer = if (currentPlayer == Stone.Black) Stone.White else Stone.Black
    selectedCoord = None
    
    resetTimer()

    // T5: Processamento Analítico de Condição de Vitória
    Konane.getWinner(currentBoard, currentPlayer, rows, cols) match {
      case Some(winner) => 
        stopTimer()
        turnLabel.setText(s"FIM DE JOGO! Vitória: $winner")
        updateBoardUI()
        showAlert("Partida Concluída", s"As peças $winner venceram o jogo! Não restam capturas legais possíveis.")
      case None => 
        turnLabel.setText(s"Turno: $currentPlayer")
        updateBoardUI()
        
        if (isVsBot && currentPlayer == Stone.White) {
          triggerBotExecution()
        }
    }
  }

  // ==========================================
  // JOGADA DO BOT: SUPORTE A NÍVEL DIFÍCIL (T7)
  // ==========================================
  def triggerBotExecution(): Unit = {
    turnLabel.setText("Computador a calcular...")
    val pause = new PauseTransition(Duration.seconds(0.7))
    
    pause.setOnFinished(_ => {
      val previousState = (currentBoard, openSpaces, currentPlayer)
      
      val (optBoard, nextRng, nextOpenSpaces) = if (botDifficulty == "Difícil") {
        // IA Minimax / Heurística de Máxima Captura: escolhe a jogada que limpa mais peças inimigas
        val allMoves = Konane.allCaptureMoves(currentBoard, currentPlayer, rows, cols)
        if (allMoves.isEmpty) (None, rng, openSpaces)
        else {
          // Ordena pela árvore que gerou maior número de peças saltadas/removidas
          val bestMove = allMoves.maxBy { case (_, _, jumped, _) => jumped.length }
          val (from, to, jumped, finalBoard) = bestMove
          val updatedOpenCoords = (from :: jumped ::: openSpaces).filter(_ != to)
          (Some(finalBoard), rng, updatedOpenCoords)
        }
      } else {
        // Nível Fácil: Delega para a função pura playRandomly (T3)
        val (b, r, o, _) = Konane.playRandomly(currentBoard, rng, currentPlayer, openSpaces, Konane.randomMove, rows, cols)
        (b, r, o)
      }

      optBoard match {
        case Some(newBoard) =>
          rng = nextRng
          history = previousState :: history
          executeGameStateTransition(newBoard, nextOpenSpaces)
        case None =>
          println("Computador sem movimentos.")
      }
    })
    pause.play()
  }

  // ==========================================
  // T8: VISUALIZAR JOGADAS VÁLIDAS (RECURSIVO)
  // ==========================================
  def updateBoardUI(): Unit = {
    boardGrid.getChildren.clear()

    // Se houver uma peça selecionada, calcula previamente todos os destinos válidos para esta peça (T8)
    val validDestinations: List[Coord2D] = selectedCoord match {
      case Some(fromCoord) =>
        Konane.allCaptureMoves(currentBoard, currentPlayer, rows, cols)
          .filter { case (start, _, _, _) => start == fromCoord }
          .map { case (_, dest, _, _) => dest }
      case None => Nil
    }

    @tailrec
    def populate(r: Int, c: Int): Unit = {
      if (r >= rows) ()
      else if (c >= cols) populate(r + 1, 0)
      else {
        val btn = new Button()
        btn.setMinSize(52, 52)
        btn.setMaxSize(52, 52)
        
        // Estilização baseada no estado de seleção ou se é uma alternativa válida (T8)
        if (selectedCoord.contains((r, c))) {
          btn.setStyle("-fx-background-color: #fffa9e; -fx-border-color: #f1c40f; -fx-border-width: 2px;")
        } else if (validDestinations.contains((r, c))) {
          // Destaca a célula vazia de destino como alternativa válida com uma borda tracejada verde
          btn.setStyle("-fx-background-color: #d5f5e3; -fx-border-color: #2ecc71; -fx-border-width: 2px; -fx-border-style: dashed;")
        } else {
          btn.setStyle("-fx-background-color: #eaeded; -fx-border-color: #bdc3c7; -fx-border-width: 0.5px;")
        }

        currentBoard.get((r, c)) match {
          case Some(Stone.Black) => 
            val circle = new Circle(16, Color.BLACK)
            btn.setGraphic(circle)
          case Some(Stone.White) => 
            val circle = new Circle(16, Color.WHITE)
            circle.setStroke(Color.BLACK)
            circle.setStrokeWidth(1.5)
            btn.setGraphic(circle)
          case None => 
            // Exibe um pequeno ponto indicador se for um destino de salto válido (Usabilidade T8)
            if (validDestinations.contains((r, c))) {
              val indicator = new Circle(4, Color.web("#2ecc71"))
              btn.setGraphic(indicator)
            }
        }

        btn.setOnAction(_ => handleCellClick(r, c))
        boardGrid.add(btn, c, r)
        populate(r, c + 1)
      }
    }

    populate(0, 0)
  }

  private def showAlert(title: String, content: String): Unit = {
    val alert = new Alert(AlertType.INFORMATION)
    alert.setTitle(title)
    alert.setHeaderText(null)
    alert.setContentText(content)
    alert.showAndWait()
  }
}

object KonaneApp {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[KonaneInterface], args*)
  }
}