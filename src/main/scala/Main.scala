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
  

  val from = (4, 2)
  val to = (2, 2)
  val captured = List((3, 2))

  val moveOption = Konane.allCaptureMoves(initialBoard, Stone.Black, 6, 6).find { case (f, t, c, _) => f == from && t == to && c == captured }

  val ((4,2), (2,2), List((3,2)), newBoard) = moveOption.get
  Konane.printBoard(newBoard, 6, 6)
  

  
  
  
  