package logic

import scala.collection.parallel.immutable.ParMap

type Coord2D = (Int, Int)
type Board = ParMap[Coord2D, Stone]

enum Stone:
  case Black, White

// T7: Configurações do Jogo Imutáveis (agora com numPlayers)
case class GameConfig(
                       rows: Int,
                       cols: Int,
                       timeLimitMs: Long,
                       difficulty: String,
                       numPlayers: Int // 1 (vs PC) ou 2 (Humano vs Humano)
                     )

// T7: Estado do jogo (com suporte para saltos intermédios/múltiplos)
case class GameState(
                      board: Board,
                      rng: MyRandom,
                      openSpaces: List[Coord2D],
                      currentPlayer: Stone,
                      midTurnPiece: Option[Coord2D] = None // Peça que está a meio de múltiplos saltos
                    )
