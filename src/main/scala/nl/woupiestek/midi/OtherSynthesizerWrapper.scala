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

class SequencerWrapper(sequencer: Sequencer = MidiSystem.getSequencer) {

  //a queue implementation
  private var in: List[Sequence] = Nil
  private var out: List[Sequence] = Nil

  private def next(): Option[Sequence] = out match {
    case head :: tail =>
      out = tail
      Some(head)
    case Nil =>
      in.reverse match {
        case head :: tail =>
          in = Nil
          out = tail
          Some(head)
        case Nil => None
      }
  }

  sequencer.addMetaEventListener(new MetaEventListener {
    override def meta(meta: MetaMessage): Unit = {
      if (meta.getType == 47) start()
    }
  })

  private def start(): Unit = next() match {
      case None => sequencer.close()
      case Some(sequence) =>
        sequencer.setSequence(sequence)
        sequencer.start()
    }

  def play(sequence: Sequence): Unit = {
    in ::= sequence
    if(!sequencer.isRunning) {
      if (!sequencer.isOpen) {
        sequencer.open()
      }
      start()
    }
  }

}
