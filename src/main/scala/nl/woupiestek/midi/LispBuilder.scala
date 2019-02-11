package nl.woupiestek.midi

import nl.woupiestek.midi.Lisp.{ Atom, Cons, Empty, Number }

sealed trait Lisp

object Lisp {

  case object Empty extends Lisp

  case class Cons(left: Lisp, right: Lisp) extends Lisp

  case class Atom(name: String) extends Lisp

  case class Number(long: Long) extends Lisp

}

sealed trait LispBuilder

object LispBuilder {

  case class Done(result: Lisp) extends LispBuilder

  case class More private (stack1: List[Lisp], stack2: List[List[Lisp]]) extends LispBuilder {

    def leftParen: LispBuilder = More(Nil, stack1 :: stack2)

    def rightParen: LispBuilder = {
      val r: Lisp = stack1.foldLeft[Lisp](Empty)((x, y) => Cons(y, x))
      stack2 match {
        case Nil => Done(r)
        case h :: t => More(r :: h, t)
      }
    }

    def atom(name: String): LispBuilder = push(Atom(name))

    def number(long: Long): LispBuilder = push(Number(long))

    private def push(lisp: Lisp) = More(lisp :: stack1, stack2)
  }

  def atom(name: String): LispBuilder = Done(Atom(name))

  def number(long: Long): LispBuilder = Done(Number(long))

  def leftParen: LispBuilder = More(Nil, Nil)

}