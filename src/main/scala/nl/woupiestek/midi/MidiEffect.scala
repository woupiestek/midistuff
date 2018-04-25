package nl.woupiestek.midi

import com.sun.media.sound.MidiUtils.META_END_OF_TRACK_TYPE
import javax.sound.midi.MidiSystem.getSequencer
import javax.sound.midi.Sequence.PPQ
import javax.sound.midi.{ MetaMessage, MidiEvent, Sequence, Sequencer }

object MidiEffect {

  def play(track: Set[MidiEvent]): Unit = {
    val sequencer: Sequencer = getSequencer
    sequencer.open()
    sequencer.addMetaEventListener((event: MetaMessage) => if (event.getType == META_END_OF_TRACK_TYPE) {
      sequencer.stop()
      sequencer.close()
    })
    sequencer.setSequence(sequence(track))
    sequencer.start()
    while (sequencer.isRunning) Thread.`yield`()
  }

  private def sequence(track: Set[MidiEvent]) = {
    val s = new Sequence(PPQ, 24)
    val t = s.createTrack()
    track.foreach(t.add)
    s
  }
}
