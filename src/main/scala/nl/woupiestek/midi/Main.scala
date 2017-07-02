package nl.woupiestek.midi

import java.io.File
import javax.sound.midi.MidiSystem.getSynthesizer
import javax.sound.midi._

import nl.woupiestek.midi.lispy.Loader
import nl.woupiestek.midi.parser.StringParser

import scala.io.Source
import scala.util.Random

object Main extends App {

  ConsoleLogic.start()

  def writeFiles(): Unit = {
    for {
      arg <- args
      sequence <- Loader.load(arg)
    } MidiSystem.write(sequence, 0, new File(s"$arg.mid"))
  }

  val wrapper = new OtherSynthesizerWrapper(getSynthesizer)

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

    wrapper.play(sequence)
  }

  def playFile(name: String): Any = {
    val input = Source.fromFile(name).getLines().mkString("\n")
    println(input)
    StringParser.parse(input, NotesAndRestsGrammar.grammar) match {
      case None => println("parsing failed")
      case Some(score) =>
        //the time unit of the synthesizer is the millisecond
        //which is too small for the files.
        val slower = score.map { case (t, e) => (100 * t, e) }
        new OtherSynthesizerWrapper(getSynthesizer).play(slower)
    }
  }

  def play(sequencer: Sequencer, sequences: Seq[Sequence]) {
    sequencer.open()
    sequencer.addMetaEventListener(new MetaEventListener() {
      def meta(event: MetaMessage): Unit = {
        if (event.getType == 47) sequencer.stop()
      }
    })
    for (sequence <- sequences) {
      sequencer.setSequence(sequence)
      sequencer.start()
      while (sequencer.isRunning) Thread.`yield`()
    }
    sequencer.close()
  }

}
