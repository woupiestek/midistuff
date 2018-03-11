package nl.woupiestek.midi.language

import javax.sound.midi.ShortMessage._
import javax.sound.midi.{ MidiEvent, ShortMessage }

sealed trait MValue

object MValue {

  case class Byte(int: Int) extends MValue

  case class Atom(name: String) extends MValue

  case class Track(events: Set[MidiEvent]) extends MValue

  case object Error extends MValue

  def atom(value: String): MValue = Atom(value)

  def string(h: String): MValue = Error

  def number(i: Int): MValue = if ((0 to 127).contains(i)) Byte(i) else Error

  private val binary: Map[String, Int] = Map(
    "on" -> NOTE_ON,
    "off" -> NOTE_OFF,
    "poly" -> POLY_PRESSURE,
    "ctrl" -> CONTROL_CHANGE,
    "bend" -> PITCH_BEND,
    "cp" -> CHANNEL_PRESSURE,
    "prog" -> PROGRAM_CHANGE)

  def list(s: List[MValue]): MValue = s match {
    case Atom(x) :: Byte(channel) :: Byte(arg0) :: Byte(arg1) :: Byte(tick) :: Nil if binary.contains(x) =>
      Track(Set(new MidiEvent(new ShortMessage(binary(x), channel, arg0, arg1), tick)))
    case Atom("join") :: t =>
      Track(t.collect { case Track(events) => events }.toSet.flatten)
    case _ => Error
  }
}
