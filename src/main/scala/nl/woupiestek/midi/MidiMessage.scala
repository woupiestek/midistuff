package nl.woupiestek.midi

import javax.sound.midi.{ Sequence, ShortMessage }

sealed trait MidiMessage

case class NoteOn(channel: Int, pitch: Int, velocity: Int) extends MidiMessage

case class NoteOff(channel: Int, pitch: Int) extends MidiMessage

case class SetProgram(channel: Int, program: Int) extends MidiMessage

object MidiMessage {

  implicit class Ops(events: List[(Int, MidiMessage)]) {
    lazy val sequence: Sequence = {
      def event(message: MidiMessage, time: Int): javax.sound.midi.MidiEvent = message match {
        case NoteOn(c, k, v) => new javax.sound.midi.MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, c, k, v), time)
        case NoteOff(c, k) => new javax.sound.midi.MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, c, k, 0), time)
        case SetProgram(c, p) => new javax.sound.midi.MidiEvent(new ShortMessage(ShortMessage.PROGRAM_CHANGE, c, p), time)
      }
      val s = new Sequence(Sequence.PPQ, 4)
      val t = s.createTrack()
      events.foreach {
        case (i, m) => t.add(event(m, i))
      }
      s
    }
  }

}