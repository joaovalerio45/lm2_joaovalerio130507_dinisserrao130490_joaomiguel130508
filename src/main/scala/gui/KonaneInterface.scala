package gui

import javafx.application.Application
import javafx.scene.Scene
import javafx.scene.layout.{BorderPane, GridPane, HBox, VBox}
import javafx.scene.control.{Button, Label, ComboBox, Alert, ButtonType}
import javafx.scene.control.Alert.AlertType
import javafx.scene.shape.Circle
import javafx.scene.paint.Color
import javafx.stage.Stage
import javafx.geometry.Insets
import javafx.geometry.Pos
import javafx.animation.{KeyFrame, Timeline, PauseTransition, Animation}
import javafx.util.Duration
import scala.annotation.tailrec
import scala.collection.parallel.immutable.ParMap

import logic.*

class KonaneInterface extends Application {
  
  // ==========================================
  // ESTADO DA INTERFACE (Imperative Shell)
  // ==========================================
  val rows = 6 
  val cols = 6 
  
  var currentBoard: Board = ParMap.empty
  var openSpaces: List[Coord2D] = Nil
  var currentPlayer: Stone = Stone.Black
  var selectedCoord: Option[Coord2D] = None
  
  var forcedCaptureCoord: Option[Coord2D] = None 
  var history: List[GameState] = Nil

  // Definições de Configuração
  var isVsBot: Boolean = false
  var rng: MyRandom = MyRandom(System.currentTimeMillis())
  var maxTimePerMove: Int = 30 
  var timeRemaining: Int = 30
  var botDifficulty: String = "Fácil" 

  // Componentes Visuais
  val boardGrid = new GridPane()
  val turnLabel = new Label("Turno: Pretas")
  val timerLabel = new Label("Tempo: 30s")
  var gameTimeline: Timeline = _
  var mainStage: Stage = _

  override def start(primaryStage: Stage): Unit = {
    mainStage = primaryStage
    showMainMenu()
  }

  // ==========================================
  // MENU CONFIGURAÇÃO PRINCIPAL (6x6)
  // ==========================================
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
    val cbTimer = new ComboBox[Int]()
    cbTimer.getItems.addAll(15, 30, 45, 60)
    cbTimer.setValue(30)
    hbTimer.getChildren.addAll(new Label("Tempo por Jogada (s):"), cbTimer)

    val btnStart = new Button("Iniciar Partida")
    btnStart.setMinSize(140, 40)
    btnStart.setStyle("-fx-base: #2ecc71; -fx-font-weight: bold;")
    
    btnStart.setOnAction(_ => {
      isVsBot = cbMode.getValue == "Jogador vs Computador"
      botDifficulty = cbDiff.getValue
      maxTimePerMove = cbTimer.getValue
      
      resetGameEngine()
      showGameView()
    })

    menuLayout.getChildren.addAll(titleLabel, hbMode, hbDiff, hbTimer, btnStart)

    val scene = new Scene(menuLayout, 450, 260)
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

    val topControls = new HBox(12)
    topControls.setPadding(new Insets(0, 0, 15, 0))
    topControls.setAlignment(Pos.CENTER_LEFT)
    
    val btnRestart = new Button("Reiniciar")
    val btnUndo = new Button("Undo (Desfazer)")
    val btnMenu = new Button("Menu Principal")
    
    topControls.getChildren.addAll(btnMenu, btnRestart, btnUndo)
    root.setTop(topControls)

    boardGrid.setHgap(6)
    boardGrid.setVgap(6)
    boardGrid.setAlignment(Pos.CENTER)
    updateBoardUI()
    root.setCenter(boardGrid)

    val statusBar = new HBox(40)
    statusBar.setPadding(new Insets(15, 0, 0, 0))
    statusBar.setAlignment(Pos.CENTER_LEFT)
    
    turnLabel.setStyle("-fx-font-weight: bold; -fx-font-size: 13px;")
    timerLabel.setStyle("-fx-font-weight: bold; -fx-font-size: 13px; -fx-text-fill: #e74c3c;")
    
    statusBar.getChildren.addAll(turnLabel, timerLabel)
    root.setBottom(statusBar)

    btnRestart.setOnAction(_ => {
      resetGameEngine()
      updateBoardUI()
    })

    btnMenu.setOnAction(_ => {
      showMainMenu()
    })

    btnUndo.setOnAction(_ => {
      Konane.undo(history) match {
        case Some(newHistory) =>
          history = newHistory
          history match {
            case head :: _ =>
              currentBoard = head.board
              currentPlayer = head.currentPlayer
              openSpaces = head.openSpaces
            case Nil =>
              resetGameEngine()
          }
          selectedCoord = None
          forcedCaptureCoord = None 
          resetTimer()
          turnLabel.setText(s"Turno: $currentPlayer")
          updateBoardUI()
          println("Undo efetuado com sucesso.")
        case None =>
          println("Histórico insuficiente para efetuar Undo.")
      }
    })

    val scene = new Scene(root, 450, 480)
    mainStage.setTitle("Kōnane - Tabuleiro 6x6")
    mainStage.setScene(scene)
    startTimer()
  }

  def resetGameEngine(): Unit = {
    currentBoard = Konane.initBoard(rows, cols)
    openSpaces = Konane.emptyCoords(currentBoard, rows, cols)
    currentPlayer = Stone.Black
    selectedCoord = None
    forcedCaptureCoord = None
    history = List(GameState(currentBoard, currentPlayer, openSpaces))
    resetTimer()
    turnLabel.setText(s"Turno: $currentPlayer")
  }

  // ==========================================
  // SISTEMA DO TEMPORIZADOR
  // ==========================================
  def startTimer(): Unit = {
    stopTimer()
    timeRemaining = maxTimePerMove
    timerLabel.setText(s"Tempo: ${timeRemaining}s")
    
    gameTimeline = new Timeline(new KeyFrame(Duration.seconds(1), _ => {
      timeRemaining -= 1
      timerLabel.setText(s"Tempo: ${timeRemaining}s")
      
      if (timeRemaining <= 0) {
        // Guarda o estado atual no histórico para permitir Undo desta perda de turno
        val previousState = GameState(currentBoard, currentPlayer, openSpaces)
        history = previousState :: history
        
        println(s"Tempo esgotado para o jogador $currentPlayer. O turno foi passado.")
        
        // Passa o turno mantendo o tabuleiro inalterado
        executeGameStateTransition(currentBoard, openSpaces)
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
    if (gameTimeline != null) gameTimeline.playFromStart()
  }

  // ==========================================
  // CAIXA DE MENSAGEM DE FIM DE JOGO
  // ==========================================
  private def showGameOverAlert(title: String, content: String): Unit = {
    val alert = new Alert(AlertType.INFORMATION)
    alert.setTitle(title)
    alert.setHeaderText("Fim da Partida!")
    alert.setContentText(content)
    
    val btnOk = new ButtonType("OK")
    alert.getButtonTypes.setAll(btnOk)
    
    alert.showAndWait()
    showMainMenu()
  }

  // ==========================================
  // LÓGICA DE SELECÇÃO E JOGADA
  // ==========================================
  def handleCellClick(r: Int, c: Int): Unit = {
    if (isVsBot && currentPlayer == Stone.White) return 

    val clickedCoord = (r, c)

    forcedCaptureCoord match {
      case Some(forcedFrom) =>
        if (forcedFrom == clickedCoord) {
          println("Tens de concluir as capturas disponíveis com esta peça ou passar o turno!")
        } else {
          executeHumanMove(forcedFrom, clickedCoord)
        }

      case None =>
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
              executeHumanMove(fromCoord, clickedCoord)
            }
        }
    }
  }

  def executeHumanMove(fromCoord: Coord2D, toCoord: Coord2D): Unit = {
    val previousState = GameState(currentBoard, currentPlayer, openSpaces)
    val (optBoard, newOpenSpaces) = Konane.play(currentBoard, currentPlayer, fromCoord, toCoord, openSpaces, rows, cols)

    optBoard match {
      case Some(newBoard) =>
        history = previousState :: history

        val remainingMovesForPiece = Konane.allCaptureMoves(newBoard, currentPlayer, rows, cols)
          .filter { case (start, _, _, _) => start == toCoord }

        if (remainingMovesForPiece.nonEmpty) {
          currentBoard = newBoard
          openSpaces = newOpenSpaces
          updateBoardUI()
          
          if (askToContinueCapturing()) {
            forcedCaptureCoord = Some(toCoord) 
            selectedCoord = Some(toCoord)
            resetTimer()
            updateBoardUI()
          } else {
            forcedCaptureCoord = None 
            executeGameStateTransition(newBoard, newOpenSpaces)
          }
        } else {
          forcedCaptureCoord = None
          executeGameStateTransition(newBoard, newOpenSpaces)
        }

      case None =>
        if (forcedCaptureCoord.isEmpty) {
          currentBoard.get(toCoord) match {
            case Some(stone) if stone == currentPlayer =>
              selectedCoord = Some(toCoord)
              updateBoardUI()
            case _ =>
              println("Movimento inválido!")
          }
        } else {
          println("Movimento inválido! Deves saltar com a peça selecionada.")
        }
    }
  }

  def askToContinueCapturing(): Boolean = {
    val alert = new Alert(AlertType.CONFIRMATION)
    alert.setTitle("Captura Múltipla")
    alert.setHeaderText("Ainda tens capturas disponíveis com esta peça!")
    alert.setContentText("Desejas continuar a capturar pedras inimigas nesta jogada?")
    
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
    forcedCaptureCoord = None 
    
    resetTimer()

    Konane.getWinner(currentBoard, currentPlayer, rows, cols) match {
      case Some(winner) => 
        stopTimer()
        turnLabel.setText(s"FIM DE JOGO! Vitória: $winner")
        updateBoardUI()
        
        val pause = new PauseTransition(Duration.seconds(0.2))
        pause.setOnFinished(_ => showGameOverAlert("Partida Concluída", s"As peças $winner venceram o jogo! Não há mais movimentos legais."))
        pause.play()
        
      case None => 
        turnLabel.setText(s"Turno: $currentPlayer")
        updateBoardUI()
        
        if (isVsBot && currentPlayer == Stone.White) {
          triggerBotExecution()
        }
    }
  }

  // ==========================================
  // JOGADA DO BOT (FÁCIL / DIFÍCIL)
  // ==========================================
  def triggerBotExecution(): Unit = {
    turnLabel.setText("Computador a calcular...")
    val pause = new PauseTransition(Duration.seconds(0.7))
    
    pause.setOnFinished(_ => {
      val previousState = GameState(currentBoard, currentPlayer, openSpaces)
      
      val (optBoard, nextRng, nextOpenSpaces) = if (botDifficulty == "Difícil") {
        val allMoves = Konane.allCaptureMoves(currentBoard, currentPlayer, rows, cols)
        if (allMoves.isEmpty) (None, rng, openSpaces)
        else {
          val bestMove = allMoves.maxBy { case (_, _, jumped, _) => jumped.length }
          val (from, to, jumped, finalBoard) = bestMove
          val updatedOpenCoords = (from :: jumped ::: openSpaces).filter(_ != to)
          (Some(finalBoard), rng, updatedOpenCoords)
        }
      } else {
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
  // VISUALIZAR ALTERNATIVAS VÁLIDAS
  // ==========================================
  def updateBoardUI(): Unit = {
    boardGrid.getChildren.clear()

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
        
        if (selectedCoord.contains((r, c))) {
          btn.setStyle("-fx-background-color: #fffa9e; -fx-border-color: #f1c40f; -fx-border-width: 2px;")
        } else if (validDestinations.contains((r, c))) {
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
}

object KonaneApp {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[KonaneInterface], args*)
  }
}