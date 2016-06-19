package nl.woupiestek.midi

import java.io.File
import javax.sound.midi._

import nl.woupiestek.midi.lispy.Loader
import nl.woupiestek.midi.parser.StringParser

import scala.io.Source
import scala.util.Random

object Main extends App {
  playSequences(MidiSystem.getSequencer, args(0).toFloat)(args.tail.flatMap(Loader.load))

  def writeFiles(): Unit = {
    for {
      arg <- args
      sequence <- Loader.load(arg)
    } MidiSystem.write(sequence, 0, new File(s"$arg.mid"))
  }

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

  def playSequences(sequencer: Sequencer, bpm: Float)(sequences: Seq[Sequence]): Unit = {
    sequencer.open()
    sequencer.setTempoInBPM(bpm)
    for (sequence <- sequences) {
      sequencer.setSequence(sequence)
      sequencer.start()
      val duration = math.ceil(sequence.getMicrosecondLength / 1000L * (120 / bpm)).toLong
      Thread.sleep(duration)
      sequencer.stop()
    }
    sequencer.close()
  }
}