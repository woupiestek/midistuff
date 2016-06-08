package nl.woupiestek.midi

sealed trait MidiEvent

case class NoteOn(channel: Int, pitch: Int, velocity: Int) extends MidiEvent

case class NoteOff(channel: Int, pitch: Int) extends MidiEvent

case class SetProgram(channel: Int, program: Int) extends MidiEvent
