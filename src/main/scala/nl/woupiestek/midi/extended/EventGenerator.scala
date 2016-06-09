package nl.woupiestek.midi.extended

import javax.sound.midi.{ShortMessage, Track}

case class EventGenerator(start: Int, heap: Map[String, ESequence], track: Track) {
  def process(eSequence: ESequence): EventGenerator = eSequence match {
    case Elements(elements) => elements.foldLeft(this) { case (state, element) => state.process(element) }
    case Put(value, key, context) => copy(heap = heap + (key -> value))
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

