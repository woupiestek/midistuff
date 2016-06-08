package nl.woupiestek.midi

import javax.sound.midi.Synthesizer

class SimpleSynthesizerWrapper(synthesizer: Synthesizer) {
  def play(sequence: List[Track]): Unit = {
    val channels = synthesizer.getChannels
    def helper(sequence: List[Track], time: Long): Unit = {
      Track.merge(sequence) match {
        case Some(track) =>
          if (time < track.time) Thread.sleep(track.time - time)
          track.event match {
            case SetProgram(channel, program) => channels(channel).programChange(program)
            case NoteOff(channel, pitch) => channels(channel).noteOff(pitch)
            case NoteOn(channel, pitch, velocity) => channels(channel).noteOn(pitch, velocity)
          }
          helper(track.subs, track.time)
        case None => ()
      }
    }
    synthesizer.open()
    helper(sequence, 0)
    synthesizer.close()
  }
}
