package it.unibo.chat.exp

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.{CustomSpawn, ScafiAlchemistSupport}

object Metrics {
  val MSGS_RECEIVED_SPAWN = "msg_received_spawn"
  val MSGS_RECEIVED_NOSPAWN = "msg_received_nospawn"
  val MSGS_SENT_SPAWN = "msg_sent_spawn"
  val MSGS_SENT_NOSPAWN = "msg_sent_nospawn"
  val TIME_TO_ARRIVE_SPAWN = "cumulative_time_spawn"
  val TIME_TO_ARRIVE_NOSPAWN = "cumulative_time_nospawn"
  val ACTIVE_PROCESSES = "n_procs_run"
  val ACTIVE_MSGS = "n_active_msgs"
  val ROUNDS_NOSPAWN = "n_rounds_nospawn"
  val BANDWIDTH_SPAWN = "bandwidth_spawn"
  val BANDWIDTH_NOSPAWN = "bandwidth_nospawn"
}

class Chat extends AggregateProgram
  with StandardSensors with ScafiAlchemistSupport with FieldUtils with CustomSpawn with BlockT with BlockG with BlockC with BlockS {
  override type MainResult = Any

  import it.unibo.Spawn._

  // FIX ISSUE IN SCAFI STDLIB
  override def randomUid: (Double, ID) = rep((thisRoundRandom), mid()) { v => (v._1, mid()) }
  def thisRoundRandom: Double = try {
    env.get[Double]("thisRoundRandom")
  } catch { case _ => env.put[Double]("thisRoundRandom",nextRandom); env.get[Double]("thisRoundRandom") }

  def delta: Int = dt(whenNan = 0).toInt

  lazy val SIM_PARAM_PROB_SEND = sense[Double]("prob_send")
  lazy val SIM_PARAM_N_DEVICES = sense[Int]("ndevices")
  lazy val SIM_PARAM_STOP_TIME = sense[Int]("stop_send_time")

  case class Msg(mid: String)(val from: ID, val to: ID){
    val sendTime: Double = currTime
    override def toString: String = s"Msg($mid)(from=$from; to=$to; sendTime=$sendTime)"
  }
  case class ChatArgs(parentToCentre: ID, dependentNodes: Set[ID])

  var receivedMsgs = Set[Msg]()

  def chatProcessLogic(msg: Msg)
                      (args: ChatArgs): (Msg,Status) = {
    val inPathFromSrcToCentre = msg.from==mid | includingSelf.anyHood {
      nbr(args.parentToCentre) == mid
    }
    val inPathFromTargetToCentre = args.dependentNodes.contains(msg.to)
    val inRegion = inPathFromSrcToCentre || inPathFromTargetToCentre

    (msg, multiBranch[Status]
      //.when(T(200,dt(whenNan=0)) <= 0){ External } // Timeout
      .when(mid == msg.to){ justOnce({
        if(!receivedMsgs.contains(msg)) {
          receivedMsgs += msg
          env.put(Metrics.MSGS_RECEIVED_SPAWN, env.get[Double](Metrics.MSGS_RECEIVED_SPAWN) + 1)
          env.put(Metrics.TIME_TO_ARRIVE_SPAWN, env.get[Double](Metrics.TIME_TO_ARRIVE_SPAWN) + (currTime - msg.sendTime))
        };Output}, thereafter = Terminated) }
      .when(mid != msg.to && inRegion){ Bubble }
      .otherwise{ External })
  }

  def chat(centre: ID, source: ID, newTargets: Set[ID]) = {
    val (distToCentre, parentToCentre) = distanceToWithParent(centre == mid)

    val dependentNodes = rep(Set.empty[ID]){ case (s: Set[ID]) =>
      excludingSelf.unionHoodSet[ID](mux( nbr{parentToCentre}==mid ){ nbr(s) }{ Set.empty[ID] }) + mid
    } // set of nodes whose path towards gen passes through me

    env.put("gradient", distToCentre)

    on(newTargets.map(t => Msg(s"$mid->$t")(source, t)))
        .withArgs(ChatArgs(parentToCentre, dependentNodes))
        .spawn(chatProcessLogic(_)).values
  }

  var receivedMsgsNoSpawn = Set[Msg]()

  def chatNoSpawn(centre: ID, source: ID, newTargets: Set[ID]) = {
    env.put(Metrics.ACTIVE_MSGS, 0.0)
    env.put(Metrics.ROUNDS_NOSPAWN, env.get[Double](Metrics.ROUNDS_NOSPAWN) + 1)
    rep((Set[Msg](), Set[Msg]())) { case (mine, removedMsgs) =>
      env.put("removed_msgs", removedMsgs)

      val nbrMsgs = includingSelf.unionHoodSet(nbr(mine))
      val allRemovedMsgs = includingSelf.unionHoodSet(nbr(removedMsgs)).intersect(nbrMsgs)
      val diff = nbrMsgs -- allRemovedMsgs

      // Messages that arrived to me are to be removed
      var toRemove = diff.filter(_.to == mid)
      toRemove.foreach(msg => {
        env.put("target", true)
        if(!receivedMsgsNoSpawn.contains(msg)) {
          env.put(Metrics.MSGS_RECEIVED_NOSPAWN, env.get[Double](Metrics.MSGS_RECEIVED_NOSPAWN) + 1)
          env.put(Metrics.TIME_TO_ARRIVE_NOSPAWN, env.get[Double](Metrics.TIME_TO_ARRIVE_NOSPAWN) + (currTime - msg.sendTime))
          env.put(s"msg_${msg.mid}", msg)
          receivedMsgsNoSpawn += msg
        }
      })

      // New messages to be propagated
      val newMsgs = newTargets.map(target => { Msg(s"${mid()}${currTime}")(mid, target) })

      // Messages that are still alive in this round
      val activeMsgs = diff -- toRemove ++ newMsgs
      env.put(Metrics.ACTIVE_MSGS, activeMsgs.size)

      val newRemovedMsgs = allRemovedMsgs ++ toRemove

      env.put(Metrics.BANDWIDTH_NOSPAWN, env.get[Double](Metrics.BANDWIDTH_NOSPAWN)
        + excludingSelf.sumHood(1) * (activeMsgs.size + newRemovedMsgs.size))

      (activeMsgs, newRemovedMsgs)
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
      env.put(Metrics.MSGS_SENT_SPAWN, env.get[Double](Metrics.MSGS_SENT_SPAWN)+1)
      env.put(Metrics.MSGS_SENT_NOSPAWN, env.get[Double](Metrics.MSGS_SENT_NOSPAWN)+1)
      newTargets += Math.round(nextRandom * SIM_PARAM_N_DEVICES).toInt
    }

//    val electedCentre = S(Double.PositiveInfinity, nbrRange)
//    val centre = broadcast(electedCentre, mid)
    val centre = 0

    env.put("centre", centre==mid)
    env.put(Metrics.ACTIVE_PROCESSES, 0.0)

    chat(centre, source, newTargets)
    chatNoSpawn(centre, source, newTargets)
  }

  /*****************************
   ** Functions **
   ******************************/

  object multiBranch {
    def apply[T] = new MultiBranchContinuation[T]()
  }
  class MultiBranchContinuation[R] { outmbranch =>
    var cases = Vector[(Boolean,()=>R)]()
    def when(m: Boolean)(f: => R) = new MultiBranchContinuation[R] { cases = outmbranch.cases :+ (m, () => f) }
    def otherwise(f: => R) = when(true)(f)
    def run(seq: Vector[(Boolean,()=>R)]): R = seq match {
      case (true,expr) +: t => branch[R](true){ expr() }{ ??? }
      case (false,_) +: t => branch[R](false){ ??? }{ run(t) }
    }
    def run: R = run(cases)
  }
  implicit def branchToValue[R](mbranch: MultiBranchContinuation[R]): R = mbranch.run

  def justOnce[T](that: T, thereafter: T): T =
    mux(rep(0)(_+1)==1){ that }{ thereafter }

  def gossipEver(x: Boolean) =
    rep(x){ old => x | includingSelf.anyHood(nbr{old}) }

  def distanceToWithParent(source: Boolean): (Double, ID) = {
    rep((Double.PositiveInfinity, -1)){ case (dist, parent) =>
      mux(source){
        (0.0, mid)
      }{
        excludingSelf.minHoodSelector(nbr{dist}+nbrRange()){
          (nbr{dist}+nbrRange(),nbr{mid})
        }.getOrElse((Double.PositiveInfinity, -1))
      }
    }
  }

  def impulsesEvery[T : Numeric](d: T, dt: T): Boolean =
    rep(false){ impulse =>
      branch(impulse) { false } { T(d,dt)==0 }
    }

  def captureChange[T](x: T) = rep((x,false)) { case (value, _) =>
    (x, value != x)
  }._2

  override def sharedTimerWithDecay[T](period: T, dt: T)(implicit ev: Numeric[T]): T =
    rep(ev.zero) { clock =>
      val clockPerceived = foldhood(clock)(ev.max)(nbr(clock))
      branch (ev.compare(clockPerceived, clock) <= 0) {
        // I'm currently as fast as the fastest device in the neighborhood, so keep on counting time
        ev.plus(clock, (branch(cyclicTimerWithDecay(period, dt)) { ev.one }  { ev.zero }))
      } {
        // Someone else's faster, take his time, and restart counting
        clockPerceived
      }
    }

  override def cyclicTimerWithDecay[T](length: T, decay: T)(implicit ev: Numeric[T]): Boolean =
    rep(length){ left =>
      branch (left == ev.zero) {
        length
      } {
        T(length, decay)
      }
    } == length
}