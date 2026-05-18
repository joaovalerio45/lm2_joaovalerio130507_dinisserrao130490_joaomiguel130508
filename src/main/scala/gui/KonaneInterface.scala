package gui

import javafx.application.Application
import javafx.scene.Scene
import javafx.scene.layout.{BorderPane, GridPane, HBox, VBox}
import javafx.scene.control.{Button, Label, ComboBox, TextField}
import javafx.scene.shape.Circle
import javafx.scene.paint.Color
import javafx.stage.Stage
import javafx.geometry.Insets
import javafx.geometry.Pos
import javafx.animation.PauseTransition
import javafx.util.Duration
import scala.annotation.tailrec
import scala.collection.parallel.immutable.ParMap

import logic.*

class KonaneInterface extends Application {
  
  // ==========================================
  // ESTADO DINÂMICO DA INTERFACE
  // ==========================================
  var rows: Int = 6
  var cols: Int = 6
  var isVsBot: Boolean = false       // Define se estamos a jogar contra o PC
  var rng: MyRandom = MyRandom(123L) // Gerador pseudoaleatório para o Bot

  var currentBoard: Board = ParMap.empty 
  var openSpaces: List[Coord2D] = Nil
  var currentPlayer: Stone = Stone.Black
  var selectedCoord: Option[Coord2D] = None

  // Componentes Visuais Principais
  val boardGrid = new GridPane()
  val turnLabel = new Label()
  var mainStage: Stage = _

  override def start(primaryStage: Stage): Unit = {
    mainStage = primaryStage
    showSizeSelectionMenu()
  }

  // ==========================================
  // MENU DE SELEÇÃO DE TAMANHO E MODO
  // ==========================================
  def showSizeSelectionMenu(): Unit = {
    val menuLayout = new VBox(20)
    menuLayout.setPadding(new Insets(30))
    menuLayout.setAlignment(Pos.CENTER)

    val titleLabel = new Label("KŌNANE - CONFIGURAÇÃO")
    titleLabel.setStyle("-fx-font-weight: bold; -fx-font-size: 16px;")

    // Inputs de Texto Customizáveis para o Tamanho
    val sizeBox = new HBox(10)
    sizeBox.setAlignment(Pos.CENTER)
    
    val rowInput = new TextField("6")
    rowInput.setPrefWidth(50)
    val colInput = new TextField("6")
    colInput.setPrefWidth(50)
    
    sizeBox.getChildren.addAll(new Label("Linhas:"), rowInput, new Label("Colunas:"), colInput)

    // Opção de Jogo (PvP ou PvE)
    val modeOptions = new ComboBox[String]()
    modeOptions.getItems.addAll("Jogador vs Jogador", "Jogador vs Computador")
    modeOptions.setValue("Jogador vs Jogador")

    val btnStart = new Button("Iniciar Jogo")
    btnStart.setMinSize(120, 35)
    
    btnStart.setOnAction(_ => {
      // Lê os valores de texto, tenta converter para Int, se falhar usa 6. 
      // Os limites .max(4) e .min(20) impedem tabuleiros muito pequenos ou absurdamente grandes.
      rows = rowInput.getText.toIntOption.getOrElse(6).max(4).min(20)
      cols = colInput.getText.toIntOption.getOrElse(6).max(4).min(20)
      
      isVsBot = modeOptions.getValue == "Jogador vs Computador"
      rng = MyRandom(1234L)
      
      initializeGameState()
      showGameView()
    })

    menuLayout.getChildren.addAll(titleLabel, sizeBox, modeOptions, btnStart)

    val scene = new Scene(menuLayout, 450, 250)
    mainStage.setTitle("Kōnane - Configuração Inicial")
    mainStage.setScene(scene)
    mainStage.show()
  }

  def initializeGameState(): Unit = {
    currentBoard = Konane.initBoard(rows, cols)
    openSpaces = Konane.emptyCoords(currentBoard, rows, cols)
    currentPlayer = Stone.Black
    selectedCoord = None
    turnLabel.setText(s"Turno: $currentPlayer")
  }

  // ==========================================
  // VISTA DO JOGO (Grelha Dinâmica e Controlos)
  // ==========================================
  def showGameView(): Unit = {
    val root = new BorderPane()
    root.setPadding(new Insets(15))

    val topControls = new HBox(15)
    topControls.setPadding(new Insets(0, 0, 15, 0))
    topControls.setAlignment(Pos.CENTER_LEFT)
    
    val btnRestart = new Button("Reiniciar")
    val btnUndo = new Button("Undo")
    val btnBackToMenu = new Button("Menu Principal")
    
    topControls.getChildren.addAll(btnBackToMenu, btnRestart, btnUndo)
    root.setTop(topControls)

    boardGrid.setHgap(5)
    boardGrid.setVgap(5)
    boardGrid.setAlignment(Pos.CENTER)
    updateBoardUI()
    root.setCenter(boardGrid)

    val statusBar = new HBox(30)
    statusBar.setPadding(new Insets(15, 0, 0, 0))
    statusBar.setAlignment(Pos.CENTER_LEFT)
    val timerLabel = new Label("Tempo: 00:00")
    
    statusBar.getChildren.addAll(turnLabel, timerLabel)
    root.setBottom(statusBar)

    // Ações dos Botões Superiores
    btnRestart.setOnAction(_ => {
      initializeGameState()
      updateBoardUI()
    })

    btnBackToMenu.setOnAction(_ => {
      showSizeSelectionMenu()
    })

    // Dimensionamento automático da janela
    val windowWidth = Math.max(cols * 58 + 40, 450)
    val windowHeight = rows * 58 + 140
    
    val scene = new Scene(root, windowWidth, windowHeight)
    mainStage.setTitle("Kōnane - Em Jogo")
    mainStage.setScene(scene)
  }

  // ==========================================
  // LÓGICA DE INTERAÇÃO HUMANA
  // ==========================================
  def handleCellClick(r: Int, c: Int): Unit = {
    // Se for contra o Bot e for a vez das Brancas, ignora os cliques do utilizador
    if (isVsBot && currentPlayer == Stone.White) return

    val clickedCoord = (r, c)

    selectedCoord match {
      case None =>
        currentBoard.get(clickedCoord) match {
          case Some(stone) if stone == currentPlayer =>
            selectedCoord = Some(clickedCoord)
            updateBoardUI()
          case _ => 
            println("Seleciona uma peça válida da tua cor.")
        }

      case Some(fromCoord) =>
        if (fromCoord == clickedCoord) {
          selectedCoord = None
          updateBoardUI()
        } else {
          val (optBoard, newOpenSpaces) = Konane.play(currentBoard, currentPlayer, fromCoord, clickedCoord, openSpaces, rows, cols)

          optBoard match {
            case Some(newBoard) =>
              processValidMove(newBoard, newOpenSpaces)
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

  // Abstraímos o pós-jogada para poder ser usado tanto pelo Humano como pelo Bot
  def processValidMove(newBoard: Board, newOpenSpaces: List[Coord2D]): Unit = {
    currentBoard = newBoard
    openSpaces = newOpenSpaces
    currentPlayer = if (currentPlayer == Stone.Black) Stone.White else Stone.Black
    selectedCoord = None
    
    Konane.getWinner(currentBoard, currentPlayer, rows, cols) match {
      case Some(winner) => 
        turnLabel.setText(s"FIM DE JOGO! Vitória: $winner")
        updateBoardUI()
      case None => 
        turnLabel.setText(s"Turno: $currentPlayer")
        updateBoardUI()
        
        // Se for modo PvE e a vez passar para o Bot, desencadeia a jogada aleatória
        if (isVsBot && currentPlayer == Stone.White) {
          triggerBotMove()
        }
    }
  }

  // ==========================================
  // LÓGICA DE JOGADA DO COMPUTADOR (BOT)
  // ==========================================
  def triggerBotMove(): Unit = {
    turnLabel.setText("Computador a pensar...")
    
    // Pequena pausa (0.6 segundos) para UX, simulando que a IA está a processar
    val pause = new PauseTransition(Duration.seconds(0.6))
    pause.setOnFinished(_ => {
      // Chama a função T3 pura que criaste no Konane.scala
      val (optBoard, nextRng, nextOpenSpaces, optDest) = 
        Konane.playRandomly(currentBoard, rng, currentPlayer, openSpaces, Konane.randomMove, rows, cols)

      optBoard match {
        case Some(newBoard) =>
          rng = nextRng
          processValidMove(newBoard, nextOpenSpaces)
        case None =>
          println("Erro: O computador não conseguiu encontrar uma jogada válida.")
      }
    })
    pause.play()
  }

  // ==========================================
  // ATUALIZAÇÃO VISUAL RECURSIVA
  // ==========================================
  def updateBoardUI(): Unit = {
    boardGrid.getChildren.clear()

    @tailrec
    def populate(r: Int, c: Int): Unit = {
      if (r >= rows) ()
      else if (c >= cols) populate(r + 1, 0)
      else {
        val btn = new Button()
        btn.setMinSize(50, 50)
        btn.setMaxSize(50, 50)
        
        if (selectedCoord.contains((r, c))) {
          btn.setStyle("-fx-background-color: #ffff99; -fx-border-color: #ffcc00; -fx-border-width: 2px;")
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