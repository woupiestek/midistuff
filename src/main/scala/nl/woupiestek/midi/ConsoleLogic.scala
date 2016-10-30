package nl.woupiestek.midi

import javax.sound.midi._

import nl.woupiestek.midi.lispy.Parser.{Get, Play, Put, Result}
import nl.woupiestek.midi.lispy.{Loader, Parser, Tokenizer, Track}
import nl.woupiestek.midi.parser.StringParser

import scala.annotation.tailrec
import scala.io.StdIn

object ConsoleLogic {


  def next(input: String, context: Map[String, Track], player: Player): Map[String, Track] = {

    @tailrec def processResult(result: Result, context: Map[String, Track]): Map[String, Track] = {
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

    StringParser.parse(input, Tokenizer.token andThen Parser.track) match {
      case None =>
        println("couldn't parse: " + input)
        context
      case Some(result) => processResult(result, context)
    }
  }

  def start() = {
    val player = new MidiPlayer()

    @tailrec def loop(context: Map[String, Track]): Unit = {
      val input = StdIn.readLine("> ")
      if (input == "exit") player.close()
      else loop(next(StdIn.readLine("> "), context, player))
    }

    loop(Map.empty)
  }

}


trait Player extends AutoCloseable {
  def play(track: Track): Unit
}

class MidiPlayer extends Player {
  val sequencer = MidiSystem.getSequencer
  sequencer.open()
  sequencer.addMetaEventListener(new MetaEventListener() {
    def meta(event: MetaMessage): Unit = {
      if (event.getType == 47) sequencer.stop()
    }
  })


  override def play(track: Track): Unit = {
    val s = new Sequence(Sequence.PPQ, 24)
    Loader.write(track, s)
    sequencer.setSequence(s)
    sequencer.start()
    while (sequencer.isRunning) Thread.`yield`()
  }

  override def close(): Unit = sequencer.close()
}

