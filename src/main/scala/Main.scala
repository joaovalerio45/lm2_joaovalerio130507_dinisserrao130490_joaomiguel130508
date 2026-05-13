import logic.*

object Main extends App:
  val rows = 6
  val cols = 6
  
  // 1. SETUP INICIAL
  val rng = MyRandom(12345L)
  val initialBoard = Konane.initBoard(rows, cols)
  val emptySpaces = Konane.emptyCoords(initialBoard, rows, cols)
  
  println("--- Jogo Konane ---")
  println("Tabuleiro Inicial:")
  println(Konane.boardToString(initialBoard, rows, cols))
  println(s"Espaços vazios iniciais: $emptySpaces\n")
  
  // 2. TESTAR JOGADA ESPECÍFICA (A mesma que tinhas)
  val from = (4, 2)
  val to = (2, 2)
  val captured = List((3, 2))

  println(s"A testar jogada das Pretas de $from para $to...")
  
  val moveOption = Konane.allCaptureMoves(initialBoard, Stone.Black, rows, cols)
    .find { case (f, t, c, _) => f == from && t == to && c == captured }

  // Usar Pattern Matching em vez do perigoso .get
  moveOption match
    case Some((_, _, _, newBoard)) =>
      println("✅ Jogada válida encontrada! Tabuleiro resultante:")
      println(Konane.boardToString(newBoard, rows, cols))
    case None =>
      println("❌ Jogada inválida. Nenhuma jogada encontrada com esses parâmetros.")

  
  
  
  