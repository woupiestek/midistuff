package nl.woupiestek.midi

import javax.sound.midi._

import nl.woupiestek.midi.injection.{Component, Container}

class OtherSynthesizerWrapper {

  val synthesizer: Component[Synthesizer] = Container.inject(classOf[Synthesizer])

  val million = 1000000l

  def play(score: List[(Int, MidiMessage)]): Component[Unit] = {
    def await(time: Long): Unit = Some((time - System.nanoTime()) / million).filter(_ > 0).foreach(Thread.sleep)
    synthesizer.map {
      synthesizer =>
        val channels = synthesizer.getChannels
        synthesizer.open()
        val ctm = System.nanoTime()
        val s2 = score.sortBy[Int](_._1).map { case (t, e) => (t * million + ctm, e) }
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

}
