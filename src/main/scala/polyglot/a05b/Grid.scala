package polyglot.a05b

import util.GridElementImpl
import util.Sequences.Sequence

import scala.util.Random


trait Grid:
  def getBombs: Sequence[GridElementImpl]

class GridImpl(val size: Int, val nBombs: Int) extends Grid:
  private var grid: Sequence[GridElementImpl] = Sequence.empty
  this.spawnBombs(this.size, this.nBombs)
  this.initializeGrid()
  private def initializeGrid(): Unit =
    (0 until size).foreach(row =>
      (0 to size).foreach(col =>
        if !this.grid.contains(grdEl => grdEl.x == row && grdEl.y == col) then
          this.grid.concat(Sequence(GridElementImpl(row, col, false)))))
  private def spawnBombs(size: Int, bombs: Int): Unit =
    var spawnedBombs = 0
    var random = Random()
    while (spawnedBombs < bombs)
      val possibleBomb = GridElementImpl(random.nextInt(size-1), random.nextInt(size-1), true)
      if (!this.grid.contains(gridEl => (gridEl.x == possibleBomb.x && gridEl.y == possibleBomb.y && gridEl.bomb == false))) then
        this.grid.concat(Sequence(possibleBomb))
        spawnedBombs = spawnedBombs + 1
  override def getBombs: Sequence[GridElementImpl] =
    this.grid.filter(grdEl => grdEl.bomb)


