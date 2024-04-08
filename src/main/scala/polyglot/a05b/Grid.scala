package polyglot.a05b

import util.Pair
import util.Sequences.Sequence


trait Grid:
  def spawnBombs(size: Int, bombs: Int): Unit
  def getBombs: Sequence[Pair]

class GridImpl(val size: Int, val nBombs: Int) extends Grid:


