package nl.woupiestek.midi

/**
  * Use a nonempty pairing heap as data structure for tracks
  * To get a monad structure, use a list of these.
  */
case class Track(time: Long, event: MidiEvent, subs: List[Track]) {
  def merge(track: Track): Track = {
    if (time <= track.time) Track(time, event, track :: subs)
    else Track(track.time, track.event, this :: track.subs)
  }

  def next: Option[Track] = Track.merge(subs)

  def shift(interval: Long): Track = Track(time + interval, event, subs.map(_.shift(interval)))
}

object Track {
  def merge(tracks: List[Track]): Option[Track] = {
    def pairwise(out: Track, in: List[Track]): Track = in match {
      case Nil => out
      case head :: Nil => out.merge(head)
      case head :: neck :: tail => pairwise(out.merge(head.merge(neck)), tail)
    }
    tracks.headOption.map(pairwise(_, tracks.tail))
  }

  def track(time: Long, event: MidiEvent): Track = Track(time, event, Nil)
}