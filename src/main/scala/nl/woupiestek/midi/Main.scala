package nl.woupiestek.midi

import javax.sound.midi._

import akka.actor.ActorRef
import nl.woupiestek.midi.extended.EventGenerator

import scala.io.Source
import scala.util.Random

object Main extends App {
  val sequencer = MidiSystem.getSequencer()
  sequencer.open()
  for (Some(sequence) <- args.map(load)) {
    sequencer.setSequence(sequence)
    sequencer.start()
    println(sequence.getMicrosecondLength)
    Thread.sleep(sequence.getMicrosecondLength / 1000L)
    sequencer.stop()
  }
  sequencer.close()

  def randomTestSounds(count: Int): Unit = {
    val random = new Random()

    val program = for (i <- 0 to 15) yield {
      Track.track(0, SetProgram(i, random.nextInt(128)))
    }

    def randomNote: List[Track] = {
      val channel = random.nextInt(16)
      val pitch = 48 + random.nextInt(32)
      val start = 100 * random.nextInt(99)
      val end = start + 250 * (1 << random.nextInt(4))
      List(Track.track(start, NoteOn(channel, pitch, 60)), Track.track(end, NoteOff(channel, pitch)))
    }

    val sequence = program.toList ++ (for {
      _ <- 1 to count
      track <- randomNote
    } yield track)

    new SimpleSynthesizerWrapper(MidiSystem.getSynthesizer).play(sequence)
  }

  def playFile(name: String) = {
    val input = Source.fromFile(name).getLines().mkString("\n")
    println(input)
    StringParser.parse(input, NotesAndRestsGrammar.grammar) match {
      case None => println("parsing failed")
      case Some(score) =>
        new OtherSynthesizerWrapper(MidiSystem.getSynthesizer).play(score, 100l)
    }
  }

  def playFile2(name: String, sequencerActorRef: ActorRef): Unit = {
    val input = Source.fromFile(name).getLines().mkString("\n")
    println("parsing...")
    StringParser.parse(input, extended.EGrammar.sequence) match {
      case None => println("parsing failed")
      case Some(eSequence) =>
        println("parsing succeeded")
        val s = EventGenerator.toMidi(eSequence)
        sequencerActorRef ! s
    }
  }

  def load(name: String): Option[Sequence] = {
    val input = Source.fromFile(name).getLines().mkString("\n")
    StringParser.parse(input, extended.EGrammar.sequence) match {
      case None =>
        println("parsing failed")
        None
      case Some(eSequence) =>
        println("parsing succeeded")
        println(eSequence)
        Some(EventGenerator.toMidi(eSequence))
    }
  }
}
