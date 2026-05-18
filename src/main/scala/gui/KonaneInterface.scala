package gui

import javafx.application.Application
import javafx.scene.Scene
import javafx.scene.layout.{BorderPane, GridPane, HBox}
import javafx.scene.control.{Button, Label}
import javafx.stage.Stage
import javafx.geometry.Insets

// A importação deve localizar-se aqui, após o package e imports do JavaFX
import logic.*

class KonaneInterface extends Application {
  override def start(primaryStage: Stage): Unit = {
    val root = new BorderPane()
    root.setPadding(new Insets(10))

    val topControls = new HBox(15)
    val btnRestart = new Button("Reiniciar")
    val btnUndo = new Button("Undo")
    topControls.getChildren.addAll(btnRestart, btnUndo)
    root.setTop(topControls)

    val boardGrid = new GridPane()
    boardGrid.setHgap(5)
    boardGrid.setVgap(5)

    // Inicialização da matriz baseada no módulo Konane
    val rows = 6
    val cols = 6
    val initialBoard = Konane.initBoard(rows, cols)

    // Iteração para desenhar o tabuleiro visual
    for (r <- 0 until rows; c <- 0 until cols) {
      
      // A classe Coord2D tem de ser instanciada com a exata capitalização
      val pieceSymbol = initialBoard(Coord2D(r, c)) match {
        case Some(Stone.Black) => "P" 
        case Some(Stone.White) => "B"
        case None => ""
      }
      
      val btn = new Button(pieceSymbol)
      btn.setMinSize(50, 50)
      
      // Captura do evento de clique para testes iniciais
      btn.setOnAction(_ => println(s"Célula clicada: Linha $r, Coluna $c"))
      
      boardGrid.add(btn, c, r) 
    }
    
    root.setCenter(boardGrid)

    val statusBar = new HBox(15)
    val turnLabel = new Label("Turno: Pretas")
    val timerLabel = new Label("Tempo: 00:00")
    statusBar.getChildren.addAll(turnLabel, timerLabel)
    root.setBottom(statusBar)

    val scene = new Scene(root, 600, 600)
    primaryStage.setTitle("Kōnane - Interface Gráfica")
    primaryStage.setScene(scene)
    primaryStage.show()
  }
}

object KonaneApp {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[KonaneInterface], args*)
  }
}