package polyglot.a05b

import polyglot.Pair
import polyglot.a05b.Logics
import polyglot.a05b.Grid

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/ */
class LogicsImpl(private val size: Int) extends Logics:

  private val grid: Grid = GridImpl(size, 5)
  private val clickedCells: Sequence[Pair[Int, Int]] = Sequence.empty()
  private val directions: Sequence[Pair[Int, Int]] = Sequence(
    Pair(-1, -1),
    Pair(-1, 0),
    Pair(-1, 1),
    Pair(0, -1),
    Pair(0, 1),
    Pair(1, 0),
    Pair(1, 1))

  override def tick(): Unit = {}

  override def isOver: Boolean = false

  override def hasElement(x: Int, y: Int): Boolean =


