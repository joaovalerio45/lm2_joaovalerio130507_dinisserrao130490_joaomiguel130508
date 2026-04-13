import logic.*

object Main extends App:
  // Inicializa a semente aleatória
  val rng = MyRandom(12345L)
  
  // Cria o tabuleiro 6x6
  val initialBoard = Konane.initBoard(6, 6)
  val emptySpaces = Konane.emptyCoords(initialBoard, 6, 6)
  
  println("--- Jogo Konane ---")
  println("Tabuleiro Inicial:")
  Konane.printBoard(initialBoard, 6, 6)
  println(s"Espaços vazios iniciais: $emptySpaces")
