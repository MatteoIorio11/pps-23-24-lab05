package polyglot.a05b

import util.GridElement
import util.Sequences.Sequence


trait Grid:
  def spawnBombs(size: Int, bombs: Int): Unit
  def getBombs: Sequence[GridElement]

class GridImpl(val size: Int, val nBombs: Int) extends Grid:
  var grid: Sequence[GridElement] = Sequence.empty
  this.initializeGrid()
  private def initializeGrid(): Unit =
    (0 until size).foreach(row =>
      (0 to size).foreach(col =>
        this.grid.concat(Sequence(GridElement(row, col, false)))))
  override def spawnBombs(size: Int, bombs: Int): Unit = ???
  override def getBombs: Sequence[GridElement] = ???


