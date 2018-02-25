package nl.woupiestek.midi.language

import javax.sound.midi.ShortMessage._

case class JMidiEvent(event: Int, arg0: Int, arg1: Int)

object JMidiEvent {

  implicit object IsEvent extends Event[JMidiEvent] {
    override def noteOn(key: Int, velocity: Int): JMidiEvent = JMidiEvent(NOTE_ON, key, velocity)

    override def noteOff(key: Int, velocity: Int): JMidiEvent = JMidiEvent(NOTE_OFF, key, velocity)

    override def keyPressure(key: Int, pressure: Int): JMidiEvent = JMidiEvent(POLY_PRESSURE, key, pressure)

    override def channelPressure(pressure: Int): JMidiEvent = JMidiEvent(CHANNEL_PRESSURE, pressure, 0)

    override def setControl(controller: Int, value: Int): JMidiEvent = JMidiEvent(CONTROL_CHANGE, controller, value)

    override def setProgram(program: Int): JMidiEvent = JMidiEvent(PROGRAM_CHANGE, program, 0)

    override def setPitchWheel(pos0: Int, pos1: Int): JMidiEvent = JMidiEvent(PITCH_BEND, pos0, pos1)
  }

  implicit object IsTrack extends Track[JMidiEvent, Set[(Int, Int, JMidiEvent)]] {
    override def single(tick: Int, channel: Int, message: JMidiEvent): Set[(Int, Int, JMidiEvent)] = Set((tick, channel, message))

    override def empty: Set[(Int, Int, JMidiEvent)] = Set.empty

    override def merge(x: Set[(Int, Int, JMidiEvent)], y: Set[(Int, Int, JMidiEvent)]): Set[(Int, Int, JMidiEvent)] = x ++ y
  }

}