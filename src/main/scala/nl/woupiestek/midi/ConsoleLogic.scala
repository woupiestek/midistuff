package nl.woupiestek.midi

import javax.sound.midi.MidiSystem.getSequencer
import javax.sound.midi._
import scalaz._
import Scalaz._
import nl.woupiestek.midi.parser.LazyMaybe
import nl.woupiestek.midi.parser.LazyMaybe._
import nl.woupiestek.midi.lispy.LParser.{ Get, Play, Put, Result }
import nl.woupiestek.midi.lispy.{ Loader, LParser, Track }

import scala.annotation.tailrec
import scala.io.StdIn

object ConsoleLogic {

  def next(
    input: String,
    context: Map[String, Track],
    player: Player): Map[String, Track] = {

    @tailrec def processResult(
      result: Result,
      context: Map[String, Track]): Map[String, Track] = {
      result match {
        case Get(key, continuation) =>
          context.get(key) match {
            case None =>
              println("key not found")
              context
            case Some(track) => processResult(continuation(track), context)
          }
        case Play(track) =>
          player.play(track)
          context
        case Put(key, track, continuation) =>
          processResult(continuation, context + (key -> track))
      }
    }

    LParser.track.parse[LazyMaybe, List, Char, Result](input.trim.toList).fold {
      println("couldn't parse: " + input)
      context
    }(processResult(_, context))

  }

  def start(): Unit = {
    val player = new MidiPlayer()

    @tailrec def loop(context: Map[String, Track]): Unit = {
      val input = StdIn.readLine("> ")
      if (null == input) ()
      else if (input == "exit") player.close()
      else loop(next(input, context, player))
    }

    println("midistuff started")
    loop(Map.empty)
  }

}

trait Player extends AutoCloseable {
  def play(track: Track): Unit
}

class MidiPlayer extends Player {
  val sequencer: Sequencer = getSequencer
  sequencer.open()
  sequencer.addMetaEventListener(
    (event: MetaMessage) => if (event.getType == 47) sequencer.stop())

  override def play(track: Track): Unit = {
    val s = new Sequence(Sequence.PPQ, 24)
    Loader.write(track, s.createTrack())
    sequencer.setSequence(s)
    sequencer.start()
  }

  override def close(): Unit = sequencer.close()
}
