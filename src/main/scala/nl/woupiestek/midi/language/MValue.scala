package nl.woupiestek.midi.language

import javax.sound.midi.ShortMessage._
import javax.sound.midi.{ MidiEvent, ShortMessage }

sealed trait MValue

object MValue {

  case class Byte(int: Int) extends MValue

  case class Atom(name: String) extends MValue

  case class Track(events: Set[MidiEvent]) extends MValue

  case object Error extends MValue

  implicit object Instance extends SExpression[MValue] {
    override def atom(value: String): MValue = Atom(value)

    override def string(h: String): MValue = Error

    override def number(i: Int): MValue = if ((0 to 127).contains(i)) Byte(i) else Error

    private val binary: Map[String, Int] = Map(
      "on" -> NOTE_ON,
      "off" -> NOTE_OFF,
      "poly" -> POLY_PRESSURE,
      "ctrl" -> CONTROL_CHANGE,
      "bend" -> PITCH_BEND,
      "cp" -> CHANNEL_PRESSURE,
      "prog" -> PROGRAM_CHANGE)

    override def list(s: List[MValue]): MValue = s match {
      case Atom(x) :: Byte(key) :: Byte(velocity) :: Byte(channel) :: Byte(tick) :: Nil if binary.contains(x) =>
        Track(Set(new MidiEvent(new ShortMessage(binary(x), channel, key, velocity), tick)))
      case Atom("join") :: t =>
        Track(t.collect { case Track(events) => events }.toSet.flatten)
      case _ => Error
    }
  }

}
