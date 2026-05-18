package logic

import scala.collection.parallel.immutable.ParMap

type Coord2D = (Int, Int)
type Board = ParMap[Coord2D, Stone]

enum Stone:
  case Black, White
case class GameConfig(
                       rows: Int,
                       cols: Int,
                       timeLimitMs: Long,
                       difficulty: String,
                       numPlayers: Int
                     )

case class GameState(
                      board: Board,
                      rng: MyRandom,
                      openSpaces: List[Coord2D],
                      currentPlayer: Stone,
                      midTurnPiece: Option[Coord2D] = None
                    )
