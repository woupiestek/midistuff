package nl.woupiestek.midi.tagless

import javax.sound.midi.ShortMessage.{ NOTE_OFF, NOTE_ON, PROGRAM_CHANGE }
import javax.sound.midi.{ MetaMessage, MidiMessage, ShortMessage }

import com.sun.media.sound.MidiUtils.META_TEMPO_TYPE

trait Track[M] {
  def noteOn(channel: Int, key: Int, velocity: Int): M

  def noteOff(channel: Int, key: Int): M

  def setProgram(channel: Int, program: Int): M

  def setTempo(bpm: Int): M

  def gained(m: M, velocity: Int): M

  def transposed(m: M, d: Int): M
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

  override def gained(m: MidiMessage, gain: Int): MidiMessage = {
    if ((m.getStatus & 0xF0) == NOTE_ON) {
      val Array(status, key, velocity) = m.getMessage
      new ShortMessage(NOTE_ON, status & 0x0F, key, velocity + gain)
    } else m
  }

  override def transposed(m: MidiMessage, d: Int): MidiMessage = m.getStatus & 0xF0 match {
    case NOTE_ON =>
      val Array(status, key, velocity) = m.getMessage
      new ShortMessage(NOTE_ON, status & 0x0F, key + d, velocity)
    case NOTE_OFF =>
      val Array(status, key) = m.getMessage
      new ShortMessage(NOTE_OFF, status & 0x0F, key + d)
    case _ => m
  }
}

