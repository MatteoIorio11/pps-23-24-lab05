package polyglot.a05b

import polyglot.Pair
import polyglot.a05b.Logics
import polyglot.a05b.Grid
import util.Sequences.Sequence
import util.Sequences.Sequence.empty

import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/ */
class LogicsImpl(private val size: Int) extends Logics:
  lazy val random: Random = Random()
  private var tickCounter: Int = 0
  private var initial: Pair[Int, Int] = Pair(random.nextInt(this.size - 2) + 1, random.nextInt(this.size - 2) + 1)

  override def tick(): Unit =
    this.tickCounter = this.tickCounter + 1

  override def isOver: Boolean =
    this.initial.getX - this.tickCounter < 0 || this.initial.getY - this.tickCounter < 0 ||
      this.initial.getX + this.tickCounter >= this.size || this.initial.getY + this.tickCounter >= this.size


  override def hasElement(x: Int, y: Int): Boolean = false



