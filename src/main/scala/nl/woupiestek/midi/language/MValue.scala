package nl.woupiestek.midi.language

import javax.sound.midi.{ MidiEvent, ShortMessage }
import javax.sound.midi.ShortMessage._

sealed trait MValue

object MValue {

  case class MList(values: List[MValue]) extends MValue

  case class Number(int: Int) extends MValue

  case class Atom(name: String) extends MValue

  case class Track(events: Set[MidiEvent]) extends MValue

  case class Closure(variable: String, body: MValue, heap: Map[String, MValue]) extends MValue

  case object Error extends MValue

  private val binary: Map[String, Int] = Map(
    "on" -> NOTE_ON,
    "off" -> NOTE_OFF,
    "poly" -> POLY_PRESSURE,
    "ctrl" -> CONTROL_CHANGE,
    "bend" -> PITCH_BEND,
    "cp" -> CHANNEL_PRESSURE,
    "prog" -> PROGRAM_CHANGE)

  private val whyIsThisProblem: PartialFunction[MValue, Set[MidiEvent]] = {
    case Track(t) => t
  }

  def evaluate(value: MValue, heap: Map[String, MValue], stack: List[MValue]): MValue = value match {
    case MList(h :: t) => evaluate(h, heap, t.map(evaluate(_, heap, Nil)) ++ stack)
    case Atom(":=") => stack match {
      case Atom(x) :: y :: z :: rest => evaluate(z, heap + (x -> y), rest)
      case _ => Error
    }
    case Atom("\\") => stack match {
      case Atom(x) :: y :: z :: rest => evaluate(y, heap + (x -> z), rest)
      case Atom(x) :: y :: Nil => Closure(x, y, heap - x)
      case _ => Error
    }
    case Atom("#") => stack match {
      case Atom(x) :: Number(channel) :: Number(arg0) :: Number(arg1) :: Number(tick) :: Nil if binary.contains(x) =>
        Track(Set(new MidiEvent(new ShortMessage(binary(x), channel % 16, arg0 % 128, arg1 % 128), tick.toLong)))
      case _ => Error
    }
    case Atom("&") => Track(stack.toSet.collect(whyIsThisProblem).flatten)
    case Atom(x) if heap.contains(x) => evaluate(heap(x), heap, stack)
    case Closure(x, y, z) => stack match {
      case h :: t => evaluate(y, heap ++ z + (x -> h), t)
      case Nil => Closure(x, y, heap ++ z - x)
    }
    case x => x
  }
}
