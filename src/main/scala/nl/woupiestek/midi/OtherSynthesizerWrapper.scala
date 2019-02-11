package nl.woupiestek.midi

import javax.sound.midi._

object OtherSynthesizerWrapper {
  implicit object MessageInstance extends MidiMessages[Synthesizer => Unit] {

    override def noteOn(channel: Int, pitch: Int, velocity: Int): (Synthesizer) => Unit =
      get(channel) andThen (_.noteOn(pitch, velocity))

    private def get(index: Int): Synthesizer => MidiChannel = {
      _.getChannels()(index)
    }

    override def noteOff(channel: Int, pitch: Int): (Synthesizer) => Unit =
      get(channel) andThen (_.noteOff(pitch))

    override def setProgram(channel: Int, program: Int): (Synthesizer) => Unit =
      get(channel) andThen (_.programChange(program))
  }
}

class OtherSynthesizerWrapper(synthesizer: Synthesizer) {

  val million = 1000000l

  def play(score: List[(Int, Synthesizer => Unit)]): Unit = {
    def await(time: Long): Unit = Some((time - System.nanoTime()) / million).filter(_ > 0).foreach(Thread.sleep)

    val ctm = System.nanoTime()
    val s2 = score.sortBy[Int](_._1).map { case (t, e) => (t * million + ctm, e) }
    synthesizer.open()
    for ((time, event) <- s2) {
      await(time)
      event(synthesizer)
    }
    synthesizer.close()
  }

}
