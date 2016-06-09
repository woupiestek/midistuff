package nl.woupiestek.midi.extended

import javax.sound.midi.{MetaEventListener, MetaMessage, MidiSystem, Sequence}

import akka.actor.Actor


class SequencerActor extends Actor {

  val sequencer = MidiSystem.getSequencer

  //a queue implementation
  class Queue() {
    var in: List[Sequence] = Nil
    var out: List[Sequence] = Nil

    def dequeue(): Option[Sequence] = out match {
      case head :: tail =>
        out = tail
        Some(head)
      case Nil =>
        in.reverse match {
          case head :: tail =>
            in = Nil
            out = tail
            Some(head)
          case Nil => None
        }
    }

    def enqueue(sequence: Sequence): Unit = in = sequence :: in
  }

  val queue = new Queue()

  import SequencerActor.Next

  sequencer.addMetaEventListener(new MetaEventListener {
    override def meta(meta: MetaMessage): Unit = {
      if (meta.getType == 47) {
        println("done playing")
        self ! SequencerActor.Next
      }
    }
  })

  override def receive: Receive = {
    case sequence: Sequence =>
      println(s"$self received sequence")
      queue.enqueue(sequence)
      if (!sequencer.isRunning) {
        if (!sequencer.isOpen) {
          sequencer.open()
        }
        self ! Next
      }
    case Next =>
      queue.dequeue() match {
        case None => sequencer.close()
        case Some(sequence) =>
          println(s"$self playing")
          sequencer.setSequence(sequence)
          sequencer.start()
      }
  }
}


object SequencerActor {

  object Next

}