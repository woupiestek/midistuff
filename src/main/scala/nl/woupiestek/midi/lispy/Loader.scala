package nl.woupiestek.midi.lispy

import javax.sound.midi.{Track => MidiTrack, _}

import nl.woupiestek.midi.lispy
import nl.woupiestek.midi.lispy.Parser.{Get, Play, Put, Result}
import nl.woupiestek.midi.parser.StringParser

import scala.annotation.tailrec
import scala.io.Source


object Loader {
  def write(track: lispy.Track, tr: MidiTrack): Unit = {
    def tempoMessage(bpm: Int): MidiMessage = {
      val data = BigInt(60000000 / bpm).toByteArray
      new MetaMessage(0x51, data, data.length)
    }

    track.events.foreach {
      case (t, m) => m match {
        case lispy.NoteOff(c, k) =>
          tr.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, c, k, 0), t))
        case lispy.NoteOn(c, k, v) =>
          tr.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, c, k, v), t))
        case lispy.ProgramChange(c, p) =>
          tr.add(new MidiEvent(new ShortMessage(ShortMessage.PROGRAM_CHANGE, c, p, 0), t))
        case lispy.Tempo(bpm) =>
          tr.add(new MidiEvent(tempoMessage(bpm), t))
      }
    }
  }

  @tailrec def extract(result: Result, context: Map[String, Track]): Option[Track] = result match {
    case Get(key, continuation) => context.get(key)
    case Play(track) => Some(track)
    case Put(key, track, continuation) => extract(continuation, context + (key -> track))
  }

  def load(name: String): Option[Sequence] = {
    val input = Source.fromFile(name).getLines().mkString("\n")
    for {
      result <- StringParser.parse(input, lispy.Parser.file)
      track <- extract(result, Map.empty)
    } yield {
      val s = new Sequence(Sequence.PPQ, 24)
      write(track, s.createTrack())
      s
    }
  }


}
