package util
import Optionals.Optional.*
import util.Optionals.Optional

import scala.annotation.tailrec

object Sequences: // Essentially, generic linkedlists
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

    override def toString: String = this match
      case Cons(h, t) => s"$h :: $t"
      case Nil() => "nil"

  object Sequence:
    def apply[A](elements: A*): Sequence[A] =
      var sequence: Sequence[A] = Nil()
      for e <- elements do
        sequence = Cons(e, sequence)
      sequence.reverse()


    def empty[A]: Sequence[A] = Nil()

    extension [A](sequence: Sequence[A])
      def head: Optional[A] = sequence match
        case Cons(h, _) => Just(h)
        case _ => Empty()

      def concat(other: Sequence[A]): Sequence[A] = sequence match
        case Cons(h, t) => Cons(h, t.concat(other))
        case _ => other

      def flatMap[B](f: A => Sequence[B]): Sequence[B] = sequence match
        case Cons(h, t) => f(h).concat(t.flatMap(f))
        case _ => Nil()

      def map[B](f: A => B): Sequence[B] = sequence.flatMap(x => Cons(f(x), Nil()))

      def filter(f: A => Boolean): Sequence[A] = sequence.flatMap:
        case x if f(x) => Cons(x, Nil())
        case _ => Nil()

      def find(f: A => Boolean): Optional[A] = sequence match
        case Cons(h, t) if f(h) => Just(h)
        case Cons(_, t) => t.find(f)
        case _ => Empty()

      def contains(e: A): Boolean = !sequence.find(_ == e).isEmpty

      def count(el: A): Int =
        @tailrec
        def _counterTail(s: Sequence[A], el: A, counter: Int): Int = s match
          case Cons(h, tail) if h == el => _counterTail(tail, el, counter + 1)
          case Cons(h, t) => _counterTail(t, el, counter)
          case Nil() => counter
        val myS = sequence
        _counterTail(myS, el, 0)

      def size(): Int =
        @tailrec
        def _countSize(seq: Sequence[A], counter: Int): Int = seq match
          case Cons(h, t) => _countSize(t, counter + 1)
          case Nil() => counter
        val myS = sequence
        _countSize(myS, 0)


      def reverse(): Sequence[A] = sequence match
        case Cons(h, t) => t.reverse().concat(Cons(h, Nil()))
        case _ => Nil()
@main def trySequences =
  import Sequences.* 
  val sequence = Sequence(1, 2, 3)
  println(sequence)
  println(sequence.head)
  println(sequence.map(_ * 2))
  println(sequence.flatMap(x => Sequence(x, x * 2)))
  println(sequence.filter(_ % 2 == 0))
  println(sequence.concat(Sequence(4, 5, 6)))
  println(sequence.find(_ % 2 == 0))
  println(sequence.contains(2))


