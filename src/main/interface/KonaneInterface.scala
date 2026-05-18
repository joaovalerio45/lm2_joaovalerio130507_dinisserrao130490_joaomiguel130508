package gui

import javafx.application.Application
import javafx.scene.Scene
import javafx.scene.layout.{BorderPane, GridPane, HBox}
import javafx.scene.control.{Button, Label}
import javafx.stage.Stage
import javafx.geometry.Insets

class KonaneInterface extends Application {
  override def start(primaryStage: Stage): Unit = {
    // Contentor principal (A tua Scene Graph base)
    val root = new BorderPane()
    root.setPadding(new Insets(10))

    // TOP: Controlos e T6 (Undo)
    val topControls = new HBox(15) // 15px de espaçamento
    val btnRestart = new Button("Reiniciar")
    val btnUndo = new Button("Undo")
    topControls.getChildren.addAll(btnRestart, btnUndo)
    root.setTop(topControls)

    // CENTER: Tabuleiro do jogo (GridPane)
    val boardGrid = new GridPane()
    boardGrid.setHgap(5)
    boardGrid.setVgap(5)
    
    // Exemplo de como renderizar uma célula vazia (linha 0, coluna 0)
    // No futuro, vais fazer um ciclo for baseado na tua matriz Konane.board
    val cell = new Button("P") 
    cell.setMinSize(50, 50)
    boardGrid.add(cell, 0, 0) 
    
    root.setCenter(boardGrid)

    // BOTTOM: Estado do Jogo e T5/T6 (Temporizador e Vitória)
    val statusBar = new HBox(15)
    val turnLabel = new Label("Turno: Pretas")
    val timerLabel = new Label("Tempo: 00:00")
    statusBar.getChildren.addAll(turnLabel, timerLabel)
    root.setBottom(statusBar)

    // Inicializar a Janela
    val scene = new Scene(root, 600, 600)
    primaryStage.setTitle("Kōnane - Interface Gráfica")
    primaryStage.setScene(scene)
    primaryStage.show()
  }
}

// Objeto App para o VS Code / Metals conseguir executar a GUI
object KonaneApp {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[KonaneGUI], args: _*)
  }
}