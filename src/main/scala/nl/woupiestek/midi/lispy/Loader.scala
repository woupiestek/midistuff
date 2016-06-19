package nl.woupiestek.midi.lispy

import javax.sound.midi.{MidiEvent, Sequence, ShortMessage}

import nl.woupiestek.midi.lispy
import nl.woupiestek.midi.parser.StringParser

import scala.io.Source


object Loader {
  def midi(track: lispy.Track): Sequence = {
    val s = new Sequence(Sequence.PPQ, 4)
    val tr = s.createTrack()
    track.events.foreach {
      case (t, m) => m match {
        case lispy.NoteOff(c, k) =>
          tr.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, c, k, 0), t))
        case lispy.NoteOn(c, k, v) =>
          tr.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, c, k, v), t))
        case lispy.ProgramChange(c,p) =>
          tr.add(new MidiEvent(new ShortMessage(ShortMessage.PROGRAM_CHANGE, c, p, 0), t))
      }
    }
    s
  }

  def load(name: String): Option[Sequence] = {
    val input = Source.fromFile(name).getLines().mkString("\n")
    StringParser.parse(input, lispy.LGrammar.file) match {
      case None =>
        println(s"parsing $name failed")
        None
      case Some(track) =>
        println(s"parsing $name succeeded")
        track.events.foreach(println)
        Some(midi(track))
    }
  }

}
