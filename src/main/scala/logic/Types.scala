package logic

import scala.collection.parallel.immutable.ParMap

type Coord2D = (Int, Int)
type Board = ParMap[Coord2D, Stone]

trait Random:
  def nextInt: (Int, Random)

enum Stone:
  case Black, White
