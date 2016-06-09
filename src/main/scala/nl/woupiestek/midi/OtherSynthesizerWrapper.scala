package nl.woupiestek.midi

import javax.sound.midi._

class OtherSynthesizerWrapper(synthesizer: Synthesizer) {

  def play(score: List[(Int, MidiEvent)], tick: Long): Unit = {
    val channels = synthesizer.getChannels
    synthesizer.open()
    val ctm = System.currentTimeMillis()
    val s2 = score.sortBy[Int](_._1).map { case (t, e) => (t * tick + ctm, e) }
    def await(time: Long): Unit = Some(time - System.currentTimeMillis()).filter(_ > 0).foreach(Thread.sleep)
    for ((time, event) <- s2) {
      await(time)
      println(event)
      event match {
        case SetProgram(channel, program) => channels(channel).programChange(program)
        case NoteOff(channel, pitch) => channels(channel).noteOff(pitch)
        case NoteOn(channel, pitch, velocity) => channels(channel).noteOn(pitch, velocity)
      }
    }
    synthesizer.close()
  }

}
