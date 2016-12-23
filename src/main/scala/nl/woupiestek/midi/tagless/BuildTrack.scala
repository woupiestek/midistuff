package nl.woupiestek.midi.tagless

import nl.woupiestek.midi.lispy._

object BuildTrack extends MidiTrack[Map[String, Track] => Track] {
  override def empty: (Map[String, Track]) => Track = _ => Track.empty

  override def setTempo(beatsPerMinute: Int): (Map[String, Track]) => Track =
    _ => Track(0, List((0, Tempo(beatsPerMinute))))

  override def note(key: Int, duration: Int): (Map[String, Track]) => Track =
    _ => Track(duration, List((0, NoteOn(0, key)), (duration, NoteOff(0, key))))

  override def rest(duration: Int): (Map[String, Track]) => Track = _ => Track(duration, Nil)

  override def setProgram(program: Int): (Map[String, Track]) => Track =
    _ => Track(0,List((0,ProgramChange(0,program))))

  override def append(list: Seq[(Map[String, Track]) => Track]): (Map[String, Track]) => Track =
    context => list.foldLeft(Track.empty){ case (x,y) => x append y(context)}

  override def stack(list: Seq[(Map[String, Track]) => Track]): (Map[String, Track]) => Track =
  context => list.foldLeft(Track.empty){ case (x,y) => x stack y(context)}

  override def piu(gain: Int, track: (Map[String, Track]) => Track): (Map[String, Track]) => Track =
    context => track(context).piu(gain)

  override def cresc(gain: Int, track: (Map[String, Track]) => Track): (Map[String, Track]) => Track =
    context => track(context).cresc(gain)

  override def setChannel(channel: Int, track: (Map[String, Track]) => Track): (Map[String, Track]) => Track =
    context => track(context).toChannel(channel)

  override def get(key: String): (Map[String, Track]) => Track = _.getOrElse(key,Track.empty)

  override def put(key: String, value: (Map[String, Track]) => Track, next: (Map[String, Track]) => Track): (Map[String, Track]) => Track =
    context => next(context + (key -> value(context)))

  override def transpose(keys: Int, track: (Map[String, Track]) => Track): (Map[String, Track]) => Track =
    context => track(context).transpose(keys)
}
