package nl.woupiestek.midi.lispy

sealed trait Message

case class Tempo(muspb: Int) extends Message

case class NoteOn(channel: Int = 0, key: Int, velocity: Int = 60) extends Message

case class NoteOff(channel: Int = 0, key: Int) extends Message

case class ProgramChange(channel: Int = 0, program: Int) extends Message

case class Track(duration: Int, events: List[(Int, Message)]) {
  def append(track: Track) =
    Track(duration + track.duration, events ++ track.events.map { case (t, m) => (t + duration, m) })

  def stack(track: Track) =
    Track(math.max(duration, track.duration), events ++ track.events)

  private def replace(f: PartialFunction[(Int, Message), Message]): Track = copy(events = events.map {
    case (t, m) if f.isDefinedAt(t, m) => (t, f(t, m))
    case x if !f.isDefinedAt(x) => x
  })

  def piu(d: Int) = replace { case (_, NoteOn(c, k, v)) => NoteOn(c, k, v + d) }

  def cresc(d: Int) = replace { case (t, NoteOn(c, k, v)) => NoteOn(c, k, v + (d * t / duration)) }

  def transpose(d: Int) = replace {
    case (_, NoteOn(c, k, v)) => NoteOn(c, k + d, v)
    case (_, NoteOff(c, k)) => NoteOff(c, k + d)
  }

  def toChannel(c: Int) = replace {
    case (_, NoteOn(_, k, v)) => NoteOn(c, k, v)
    case (_, NoteOff(_, k)) => NoteOff(c, k)
    case (_, ProgramChange(_, p)) => ProgramChange(c, p)
  }
}

object Track {
  def empty = Track(0, Nil)
}

