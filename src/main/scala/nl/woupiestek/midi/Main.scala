package nl.woupiestek.midi

import java.io.File

import javax.sound.midi.MidiSystem.getSynthesizer
import javax.sound.midi._
import nl.woupiestek.midi.OtherSynthesizerWrapper.MessageInstance
import nl.woupiestek.midi.lispy.Loader

import scala.util.Random

object Main extends App {

  ConsoleLogic.start()

  def writeFiles(): Unit = for {
    arg <- args
    sequence <- Loader.load(arg)
  } MidiSystem.write(sequence, 0, new File(s"$arg.mid"))

  private def randomTestSounds[M](count: Int)(implicit M: MidiMessage[M]) = {
    val random = new Random()
    val program = for (i <- 0 to 15) yield (0, M.setProgram(i, random.nextInt(128)))

    def randomNote: List[(Int, M)] = {
      val channel = random.nextInt(16)
      val pitch = 48 + random.nextInt(32)
      val start = 100 * random.nextInt(99)
      val end = start + 250 * (1 << random.nextInt(4))
      List((start, M.noteOn(channel, pitch, 60)), (end, M.noteOff(channel, pitch)))
    }

    program.toList ++ (for {
      _ <- 1 to count
      track <- randomNote
    } yield track)
  }

  def playRandom(count: Int): Unit =
    new OtherSynthesizerWrapper(getSynthesizer).play(randomTestSounds(count)(MessageInstance))

}
