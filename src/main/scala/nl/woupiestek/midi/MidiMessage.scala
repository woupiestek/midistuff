package nl.woupiestek.midi

sealed trait MidiMessage

case class NoteOn(channel: Int, pitch: Int, velocity: Int) extends MidiMessage

case class NoteOff(channel: Int, pitch: Int) extends MidiMessage

case class SetProgram(channel: Int, program: Int) extends MidiMessage
