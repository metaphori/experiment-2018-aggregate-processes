package chat.exp

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

class ChatNoSpawn extends AggregateProgram
  with StandardSensors with ScafiAlchemistSupport with FieldUtils with CustomSpawn with BlockT with BlockG with BlockC {
  override type MainResult = Any

  val centre = 189

  def delta: Int = dt(whenNan = 0).toInt

  val SIM_METRIC_N_MSGS_RECEIVED = "msg_received"
  val SIM_METRIC_N_MSGS_SENT = "msg_sent"
  val SIM_METRIC_TIME_TO_ARRIVE = "cumulative_time"

  lazy val SIM_PARAM_PROB_SEND = sense[Double]("prob_send")
  lazy val SIM_PARAM_N_DEVICES = sense[Int]("ndevices")
  lazy val SIM_PARAM_STOP_TIME = sense[Int]("stop_send_time")

  case class Msg(mid: String)(val from: ID, val to: ID, val sendTime: Double){
    override def toString: String = s"Msg($mid)(from=$from; to=$to; sendTime=$sendTime)"
  }

  var receivedMsgs = Set[Msg]()

  def chat(centre: ID, source: ID, newTargets: Set[ID]) = {
    rep((Set[Msg](), Set[Msg]())) { case (mine, removedMsgs) =>
      env.put("removed_msgs", removedMsgs)

      val nbrMsgs = includingSelf.unionHoodSet(nbr(mine))
      val allRemovedMsgs = includingSelf.unionHoodSet(nbr(removedMsgs))
      val diff = nbrMsgs -- allRemovedMsgs

      // Messages that arrived to me are to be removed
      var toRemove = diff.filter(_.to == mid)
      toRemove.foreach(msg => {
        env.put("target", true)
        env.put(SIM_METRIC_N_MSGS_RECEIVED, env.get[Double](SIM_METRIC_N_MSGS_RECEIVED) + 1)
        env.put(SIM_METRIC_TIME_TO_ARRIVE, env.get[Double](SIM_METRIC_TIME_TO_ARRIVE) + (currTime - msg.sendTime))
        env.put(s"msg_${msg.mid}", msg)
      })

      // New messages to be propagated
      val newMsgs = newTargets.map(target => {
        env.put(SIM_METRIC_N_MSGS_SENT, env.get[Double](SIM_METRIC_N_MSGS_SENT) + 1)
        Msg(s"${mid()}${currTime}")(mid, target, currTime)
      })

      // Messages that are still alive in this round
      val activeMsgs = diff -- toRemove ++ newMsgs
      env.put(SIM_METRIC_N_PROCS_RUN, activeMsgs.size)

      // Garbage collection
      val garbage = includingSelf.intersectionHoodSet(nbr(removedMsgs))

      (activeMsgs -- garbage, allRemovedMsgs -- garbage ++ toRemove)
    }
  }

  implicit class MyRichFieldOps(fo: FieldOps) {
    import fo._
    def intersectionHoodSet[T](expr: => Iterable[T]): Set[T] = {
      var first = true
      foldhoodTemplate[Set[T]](Set())((acc,x) => if(first){ first = false; x } else acc.intersect(x))(expr.toSet)
    }
  }

  /*****************************
  ** MAIN PROGRAM ENTRY POINT **
  ******************************/

  override def main() = {
    var newTargets = Set[ID]()
    val source: ID = if(nextRandom<SIM_PARAM_PROB_SEND && currTime<SIM_PARAM_STOP_TIME) mid else -1

    env.put("target", false)
    env.put("source", source==mid)

    if(source==mid) {
      env.put(SIM_METRIC_N_MSGS_SENT, env.get[Double](SIM_METRIC_N_MSGS_SENT)+1)
      newTargets += Math.round(nextRandom * SIM_PARAM_N_DEVICES).toInt
    }

    env.put("centre", centre==mid)
    env.put(SIM_METRIC_N_PROCS_RUN, 0.0)

    chat(centre, source, newTargets)
  }
}