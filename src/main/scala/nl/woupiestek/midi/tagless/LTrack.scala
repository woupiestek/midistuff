package nl.woupiestek.midi.tagless

import javax.sound.midi.{Track => MidiTrack, _}

import com.sun.media.sound.MidiUtils

trait Message[M] {
  def noteOn(channel: Int, key: Int, velocity: Int): M

  def noteOff(channel: Int, key: Int): M

  def setProgram(channel: Int, program: Int): M

  def setTempo(bpm: Int): M
}

object MessageInstance extends Message[MidiMessage] {
  override def noteOn(channel: Int, key: Int, velocity: Int): MidiMessage =
    new ShortMessage(ShortMessage.NOTE_ON, channel, key, velocity)

  override def noteOff(channel: Int, key: Int): MidiMessage =
    new ShortMessage(ShortMessage.NOTE_OFF, channel, key, 0)

  override def setProgram(channel: Int, program: Int): MidiMessage =
    new ShortMessage(ShortMessage.PROGRAM_CHANGE, channel, program, 0)

  override def setTempo(bpm: Int): MidiMessage = {
    val data = BigInt(60000000 / bpm).toByteArray
    new MetaMessage(MidiUtils.META_TEMPO_TYPE, data, data.length)
  }
}

trait Track[T, M] {
  def add(message: M, time: Long, track: T): T
}

object TrackInstance extends Track[MidiTrack => Unit, MidiMessage] {
  override def add(message: MidiMessage, time: Long, track: (MidiTrack) => Unit): (MidiTrack) => Unit = x => {
    x.add(new MidiEvent(message, time))
    track(x)
  }
}

