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

  case class Msg(str: String, sendTime: Double)

  var receivedMsgs = Set[Msg]()

  def chat(centre: ID, source: ID, newTargets: Set[ID]) = {
    type InitParams = (ID, ID, Msg)  // source, target, msg
    type RuntimeParams = (Double, ID, Set[ID])  // dist to centre, parent to centre, set of nodes
    type Result = Msg

    val chatComputation: InitParams => RuntimeParams => (Result, Status) = {
      case (src: ID, target: ID, msg: Msg) => { case (distToCentre, parentToCentre, dependentNodes) => {
        val distToSource = distanceTo(src == mid)
        val (distToTarget, parentInPathToTarget) = distanceToWithParent(target == mid) // distance and direction to target

        val inPathFromSrcToCentre = src==mid | includingSelf.anyHood {
          nbr(parentToCentre) == mid
        } // am I in path from src to centre?
        val inPathFromTargetToCentre = dependentNodes.contains(target) // am I in path from target to centre?
        //val middle = anyHood(distToSource + nbrRange < nbr(distToSource)) // do I improve distance src-target?
        val inRegion = inPathFromSrcToCentre || inPathFromTargetToCentre // || middle

        val status: Status = branch(mid == target) {
          env.put("target", true)
          mux[Status](rep(0)(_+1)==1) {
            if(!receivedMsgs.contains(msg)) {
              receivedMsgs += msg
              env.put(SIM_METRIC_N_MSGS_RECEIVED, env.get[Double](SIM_METRIC_N_MSGS_RECEIVED) + 1)
              env.put(SIM_METRIC_TIME_TO_ARRIVE, env.get[Double](SIM_METRIC_TIME_TO_ARRIVE) + (currTime - msg.sendTime))
            }
            Output
          } {
            Terminated
          }
        } { if (inRegion) {
          Bubble
        } else {
          External
        } }

        (msg, status)
      }
    } }

    val (distToCentre, parentToCentre) = distanceToWithParent(centre == mid)

    val dependentNodes = rep(Set[ID]()){ case (s: Set[ID]) =>
      excludingSelf.unionHoodSet[ID](mux( nbr{parentToCentre}==mid ){ nbr(s) }{ Set[ID]() }) + mid
    } // set of nodes whose path towards gen passes through me

    env.put("gradient", distToCentre)

    val targets_found: Map[InitParams, Result] =
      spawn[InitParams,RuntimeParams,Result](chatComputation,
        newTargets.map(t => (source, t, Msg(s"Msg from $mid to $t", currTime))),
        (distToCentre, parentToCentre, dependentNodes))
    targets_found
  }

  /*****************************
  ** MAIN PROGRAM ENTRY POINT **
  ******************************/

  override def main() = {
    var newTargets = Set[ID]()
    val source: ID = if(nextRandom<SIM_PARAM_PROB_SEND) mid else -1

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