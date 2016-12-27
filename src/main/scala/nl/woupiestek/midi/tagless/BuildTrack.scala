package nl.woupiestek.midi.tagless

import nl.woupiestek.midi.lispy.{Track => LTrack,_}

object BuildTrack extends Score[LTrack] {
  override def empty: LTrack = LTrack.empty

  override def setTempo(beatsPerMinute: Int): LTrack = LTrack(0, List((0, Tempo(beatsPerMinute))))

  override def note(key: Int, duration: Int): LTrack = LTrack(duration, List((0, NoteOn(0, key)), (duration, NoteOff(0, key))))

  override def rest(duration: Int): LTrack = LTrack(duration, Nil)

  override def setProgram(program: Int): LTrack = LTrack(0, List((0, ProgramChange(0, program))))

  override def append(list: Seq[LTrack]): LTrack = list.foldLeft(LTrack.empty) { case (x, y) => x append y }

  override def stack(list: Seq[LTrack]): LTrack = list.foldLeft(LTrack.empty) { case (x, y) => x stack y }

  override def piu(gain: Int, track: LTrack): LTrack = track.piu(gain)

  override def cresc(gain: Int, track: LTrack): LTrack = track.cresc(gain)

  override def setChannel(channel: Int, track: LTrack): LTrack = track.toChannel(channel)

  override def transpose(keys: Int, track: LTrack): LTrack = track.transpose(keys)
}
