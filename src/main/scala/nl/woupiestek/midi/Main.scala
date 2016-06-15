package nl.woupiestek.midi

import javax.sound.midi._

import akka.actor.ActorRef
import nl.woupiestek.midi.extended.EventGenerator
import nl.woupiestek.midi.lispy.Loader
import nl.woupiestek.midi.parser.StringParser

import scala.io.Source
import scala.util.Random

object Main extends App {
  playSequences(MidiSystem.getSequencer, args.flatMap(Loader.load))

  def randomTestSounds(count: Int): Unit = {
    val random = new Random()

    val program = for (i <- 0 to 15) yield (0, SetProgram(i, random.nextInt(128)))

    def randomNote: List[(Int, MidiMessage)] = {
      val channel = random.nextInt(16)
      val pitch = 48 + random.nextInt(32)
      val start = 100 * random.nextInt(99)
      val end = start + 250 * (1 << random.nextInt(4))
      List((start, NoteOn(channel, pitch, 60)), (end, NoteOff(channel, pitch)))
    }

    val sequence = program.toList ++ (for {
      _ <- 1 to count
      track <- randomNote
    } yield track)

    new OtherSynthesizerWrapper(MidiSystem.getSynthesizer).play(sequence, 100l)
  }

  def playFile(name: String) = {
    val input = Source.fromFile(name).getLines().mkString("\n")
    println(input)
    StringParser.parse(input, NotesAndRestsGrammar.grammar) match {
      case None => println("parsing failed")
      case Some(score) => new OtherSynthesizerWrapper(MidiSystem.getSynthesizer).play(score, 100l)
    }
  }

  def playFile2(name: String, sequencerActorRef: ActorRef): Unit =
    EventGenerator.load(name).foreach(sequencerActorRef ! _)

  def playSequences(sequencer: Sequencer, sequences: Seq[Sequence]): Unit = {
    sequencer.open()
    for (sequence <- sequences) {
      sequencer.setSequence(sequence)
      sequencer.start()
      Thread.sleep(sequence.getMicrosecondLength / 1000L)
      sequencer.stop()
    }
    sequencer.close()
  }
}
