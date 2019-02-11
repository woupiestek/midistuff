package nl.woupiestek.midi

import com.sun.media.sound.MidiUtils.META_END_OF_TRACK_TYPE
import javax.sound.midi.MidiSystem.getSequencer
import javax.sound.midi.Sequence.PPQ
import javax.sound.midi.ShortMessage._
import javax.sound.midi._

object MidiEffect {

  def play(sequence: Sequence): Unit = {
    val sequencer: Sequencer = getSequencer
    sequencer.open()
    sequencer.addMetaEventListener((event: MetaMessage) => if (event.getType == META_END_OF_TRACK_TYPE) {
      sequencer.stop()
      sequencer.close()
    })
    sequencer.setSequence(sequence)
    sequencer.start()
  }

  def sequence(track: Set[MidiEvent]): Sequence = {
    val s = new Sequence(PPQ, 24)
    val t = s.createTrack()
    track.foreach(t.add)
    s
  }

  def shortEvent(command: Command, data1: Int, data2: Int, tick: Long): MidiEvent =
    new MidiEvent(new ShortMessage(command.code, data1, data2), tick)
}

case class Command private (code: Int)

object Command {
  val noteOff = Command(NOTE_OFF)
  val noteOn = Command(NOTE_ON)
  val polyPressure = Command(POLY_PRESSURE)
  val controlChange = Command(CONTROL_CHANGE)
  val programChange = Command(PROGRAM_CHANGE)
  val channelPressure = Command(CHANNEL_PRESSURE)
  val pitchBend = Command(PITCH_BEND)
  val values = Set(noteOff, noteOn, polyPressure, controlChange, programChange, channelPressure, pitchBend)
}

