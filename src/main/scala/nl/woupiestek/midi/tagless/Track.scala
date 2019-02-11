package nl.woupiestek.midi.tagless

import javax.sound.midi.ShortMessage.{ NOTE_OFF, NOTE_ON, PROGRAM_CHANGE }
import javax.sound.midi.{ MetaMessage, MidiMessage, ShortMessage }

import com.sun.media.sound.MidiUtils.META_TEMPO_TYPE

object Track {

  def noteOn(channel: Int, key: Int, velocity: Int): MidiMessage =
    new ShortMessage(NOTE_ON, channel, key, velocity)

  def noteOff(channel: Int, key: Int): MidiMessage =
    new ShortMessage(NOTE_OFF, channel, key, 0)

  def setProgram(channel: Int, program: Int): MidiMessage =
    new ShortMessage(PROGRAM_CHANGE, channel, program, 0)

  def setTempo(bpm: Int): MidiMessage = {
    val data = BigInt(60000000 / bpm).toByteArray
    new MetaMessage(META_TEMPO_TYPE, data, data.length)
  }

  def gained(m: MidiMessage, gain: Int): MidiMessage = {
    if ((m.getStatus & 0xF0) == NOTE_ON) {
      val Array(status, key, velocity) = m.getMessage
      new ShortMessage(NOTE_ON, status & 0x0F, key.toInt, velocity + gain)
    } else m
  }

  def transposed(m: MidiMessage, d: Int): MidiMessage =
    m.getStatus & 0xF0 match {
      case NOTE_ON | NOTE_OFF =>
        val Array(status, key, velocity) = m.getMessage
        new ShortMessage(status.toInt, key + d, velocity.toInt)
      case _ => m
    }
}
