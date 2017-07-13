package nl.woupiestek.midi.tagless

import javax.sound.midi.ShortMessage.{ NOTE_OFF, NOTE_ON, PROGRAM_CHANGE }
import javax.sound.midi.{ MetaMessage, MidiMessage, ShortMessage }

import com.sun.media.sound.MidiUtils.META_TEMPO_TYPE

trait Track[M] {
  def noteOn(channel: Int, key: Int, velocity: Int): M

  def noteOff(channel: Int, key: Int): M

  def setProgram(channel: Int, program: Int): M

  def setTempo(bpm: Int): M
}

object TrackInstance extends Track[MidiMessage] {

  override def noteOn(channel: Int, key: Int, velocity: Int): MidiMessage =
    new ShortMessage(NOTE_ON, channel, key, velocity)

  override def noteOff(channel: Int, key: Int): MidiMessage =
    new ShortMessage(NOTE_OFF, channel, key, 0)

  override def setProgram(channel: Int, program: Int): MidiMessage =
    new ShortMessage(PROGRAM_CHANGE, channel, program, 0)

  override def setTempo(bpm: Int): MidiMessage = {
    val data = BigInt(60000000 / bpm).toByteArray
    new MetaMessage(META_TEMPO_TYPE, data, data.length)
  }
}

