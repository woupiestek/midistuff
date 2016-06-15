package nl.woupiestek.midi.extended

import javax.sound.midi.{Sequence, ShortMessage, Track}

import nl.woupiestek.midi.extended
import nl.woupiestek.midi.parser.StringParser

import scala.io.Source

final case class EventGenerator(start: Int, heap: Map[String, ESequence], track: Track) {
  def process(eSequence: ESequence): EventGenerator = eSequence match {
    case Elements(elements) => elements.foldLeft(this) { case (state, element) => state.process(element) }
    case Put(value, key, context) => copy(heap = heap + (key -> value)).process(context)
  }

  def process(element: ESequence.Element): EventGenerator = element match {
    case ESequence.Note(key, duration) =>
      track.add(new javax.sound.midi.MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, 0, key, 60), start))
      val stop = start + duration
      track.add(new javax.sound.midi.MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, 0, key, 60), stop))
      copy(start = stop)
    case ESequence.Rest(duration) =>
      val stop = start + duration
      copy(start = stop)
    case ESequence.Poly(eseqs) =>
      val stops = eseqs.map(eseq => process(eseq).start)
      copy(start = (start :: stops).max)
    case ESequence.Get(key: String) => process(heap(key))
  }
}

object EventGenerator {

  def load(name: String): Option[Sequence] = {
    val input = Source.fromFile(name).getLines().mkString("\n")
    StringParser.parse(input, extended.EGrammar.sequence) match {
      case None =>
        println(s"parsing $name failed")
        None
      case Some(eSequence) =>
        println(s"parsing $name succeeded")
        println(eSequence)
        Some(toMidi(eSequence))
    }
  }

  def toMidi(eSequence: ESequence): Sequence = {
    val s = new Sequence(Sequence.PPQ, 4)
    EventGenerator(0, Map.empty, s.createTrack()).process(eSequence)
    s
  }
}

