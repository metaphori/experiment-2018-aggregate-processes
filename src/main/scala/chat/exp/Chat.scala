package chat.exp

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

class Chat extends AggregateProgram
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

  case class Msg(src: ID, target: ID, str: String) {
    val sendTime: Double = currTime
  }
  case class ChatArgs(distToCentre: Double, parentToCentre: ID, dependentNodes: Set[ID])

  var receivedMsgs = Set[Msg]()

  def chatProcessLogic(msg: Msg)
                      (args: ChatArgs): (Msg,Status) = {
    val inPathFromSrcToCentre = msg.src==mid | includingSelf.anyHood {
      nbr(args.parentToCentre) == mid
    }
    val inPathFromTargetToCentre = args.dependentNodes.contains(msg.target)
    val inRegion = inPathFromSrcToCentre || inPathFromTargetToCentre

    (msg, multiBranch[Status]
      .when(mid == msg.target){ mux[Status](rep(0)(_+1)==1){
        if(!receivedMsgs.contains(msg)) {
          receivedMsgs += msg
          env.put(SIM_METRIC_N_MSGS_RECEIVED, env.get[Double](SIM_METRIC_N_MSGS_RECEIVED) + 1)
          env.put(SIM_METRIC_TIME_TO_ARRIVE, env.get[Double](SIM_METRIC_TIME_TO_ARRIVE) + (currTime - msg.sendTime))
        };Output}{Terminated} }
      .when(mid != msg.target && inRegion){ Bubble }
      .otherwise{ External })
  }

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

  def chat(centre: ID, source: ID, newTargets: Set[ID]) = {
    val (distToCentre, parentToCentre) = distanceToWithParent(centre == mid)

    val dependentNodes = rep(Set.empty[ID]){ case (s: Set[ID]) =>
      excludingSelf.unionHoodSet[ID](mux( nbr{parentToCentre}==mid ){ nbr(s) }{ Set.empty[ID] }) + mid
    } // set of nodes whose path towards gen passes through me

    env.put("gradient", distToCentre)

    on(newTargets.map(t => Msg(source, t, s"Msg from $mid to $t")))
        .withArgs(ChatArgs(distToCentre, parentToCentre, dependentNodes))
        .spawn(chatProcessLogic(_)).values
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

  /*****************************
   ** Functions **
   ******************************/

  def gossipEver(x: Boolean) =
    rep(x){ old => x | includingSelf.anyHood(nbr{old}) }

  def distanceToWithParent(source: Boolean): (Double, ID) = {
    rep((Double.PositiveInfinity, mid)){ case (dist, parent) =>
      mux(source){
        (0.0, mid)
      }{
        excludingSelf.minHoodSelector(nbr{dist}+nbrRange()){
          (nbr{dist}+nbrRange(),nbr{mid})
        }.getOrElse((Double.PositiveInfinity, mid))
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