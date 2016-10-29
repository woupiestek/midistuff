package nl.woupiestek.midi

import javax.sound.midi.Sequence

import nl.woupiestek.midi.lispy.{Loader, Parser, Tokenizer, Track}
import nl.woupiestek.midi.parser.StringParser

object ConsoleLogic {

  def next(input: String, context: Map[String, Track]): Map[String, Track] = {
    StringParser.parse(input, Tokenizer.token andThen Parser.track(context)) match {
      case None =>
        println("couldn't parse: " + input)
        context
      case Some(result) =>
        val s = new Sequence(Sequence.PPQ, 24)
        Loader.write(result.track,s)
        println(s)
        result.context
    }
  }

}
