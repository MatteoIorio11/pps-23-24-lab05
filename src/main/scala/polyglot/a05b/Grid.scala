package polyglot.a05b

import util.Optionals.Optional
import util.Optionals.Optional.Just
import util.Sequences.Sequence

import scala.util.Random


trait Grid:
  def getBombs: Sequence[GridElementImpl]
  def checkBomb(x: Int, y: Int): Boolean

class GridImpl(val size: Int, val nBombs: Int) extends Grid:
  private var grid: Sequence[GridElementImpl] = Sequence.empty
  this.spawnBombs(this.size, this.nBombs)
  this.initializeGrid()
  private def initializeGrid(): Unit =
    (0 until size).foreach(row =>
      (0 to size).foreach(col =>
        if this.grid.find(grdEl => grdEl.x == row && grdEl.y == col).isEmpty then
          this.grid = this.grid.concat(Sequence(new GridElementImpl(row, col, false)))))
  private def spawnBombs(size: Int, bombs: Int): Unit =
    var spawnedBombs = 0
    var random = Random()
    while (spawnedBombs < bombs)
      val possibleBomb = new GridElementImpl(random.nextInt(size-1), random.nextInt(size-1), true)
      if this.grid.find(gridEl => gridEl.x == possibleBomb.x && gridEl.y == possibleBomb.y && !gridEl.bomb).isEmpty then
        this.grid = this.grid.concat(Sequence(possibleBomb))
        spawnedBombs = spawnedBombs + 1

  override def checkBomb(x: Int, y: Int): Boolean =
    val grdEl: Optional[GridElementImpl] = this.grid.find(grdEl => grdEl.x == x && grdEl.y == y)
    grdEl match
      case Just(cell) => cell.bomb
  override def getBombs: Sequence[GridElementImpl] =
    this.grid.filter(grdEl => grdEl.bomb)

class GridElementImpl(val x: Int, val y: Int, val bomb: Boolean)

