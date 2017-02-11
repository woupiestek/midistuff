package nl.woupiestek.midi

import javax.sound.midi._

import nl.woupiestek.midi.injection.{ Component, Container }

class OtherSynthesizerWrapper {

  val synthesizer: Component[Synthesizer] = Container.inject[Synthesizer]

  val million = 1000000l

  def play(score: List[(Int, MidiMessage)]): Component[Unit] = {
    def await(time: Long): Unit = Some((time - System.nanoTime()) / million).filter(_ > 0).foreach(Thread.sleep)
    def on(s2: List[(Long, MidiMessage)])(synthesizer: Synthesizer): Unit = {
      val channels = synthesizer.getChannels
      synthesizer.open()
      for ((time, event) <- s2) {
        await(time)
        event match {
          case SetProgram(channel, program) => channels(channel).programChange(program)
          case NoteOff(channel, pitch) => channels(channel).noteOff(pitch)
          case NoteOn(channel, pitch, velocity) => channels(channel).noteOn(pitch, velocity)
        }
      }
      synthesizer.close()
    }
    val ctm = System.nanoTime()
    val s2 = score.sortBy[Int](_._1).map { case (t, e) => (t * million + ctm, e) }
    synthesizer.map(on(s2))
  }

}
