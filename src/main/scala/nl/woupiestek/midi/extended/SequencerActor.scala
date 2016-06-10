package nl.woupiestek.midi.extended

import javax.sound.midi._

import akka.actor.Actor
import akka.event.Logging


class SequencerActor(sequencer: Sequencer) extends Actor {

  import SequencerActor._

  val logger = Logging.getLogger(context.system, this)

  var q = new Q()

  override def receive: Receive = {
    case sequence: Sequence =>
      logger.info(s"received sequence")
      q = q.append(sequence)
      if (!sequencer.isRunning) {
        self ! Next
      }
    case Next => q.unfold match {
      case None =>
        sequencer.close()
        q = new Q()
      case Some((s, q2)) =>
        sequencer.setSequence(s)
        if (!sequencer.isOpen) sequencer.open()
        sequencer.start()
        q = q2
        Thread.sleep(s.getMicrosecondLength / 1000L)
        self ! Next
    }
  }
}


object SequencerActor {

  class Q(in: List[Sequence] = Nil, out: List[Sequence] = Nil) {
    def append(s: Sequence): Q = new Q(s :: in, out)

    lazy val unfold: Option[(Sequence, Q)] = out match {
      case h :: t => Some((h, new Q(in, out)))
      case Nil => in.reverse match {
        case h :: t => Some((h, new Q(Nil, t)))
        case Nil => None
      }
    }
  }

  object Next

}