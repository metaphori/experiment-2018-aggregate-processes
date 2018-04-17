package it.unibo.chat.basics

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.{CustomSpawn, ScafiAlchemistSupport}

class Chat extends AggregateProgram
  with StandardSensors with ScafiAlchemistSupport with FieldUtils with CustomSpawn with BlockT with BlockG with BlockC {
  override type MainResult = Any

  import Status._

  val centre = 189
  val targets = Vector(200,77)
  val source = 22

  val startEvery = 20 // A generation impulse every 20 time units

  def delta: Int = dt(whenNan = 0).toInt

  def chat(centre: ID, newTargets: Set[ID]) = {
    type InitParams = (ID, ID, String)  // source, target, msg
    type RuntimeParams = (Double, ID, Set[ID])  // dist to centre, parent to centre, set of nodes
    type Result = String

    val chatComputation: InitParams => RuntimeParams => (Result, Status) = {
      case (src: ID, target: ID, msg: String) => { case (distToCentre, parentToCentre, dependentNodes) => {
        val distToSource = distanceTo(src == mid)
        val (distToTarget, parentInPathToTarget) = distanceToWithParent(target == mid) // distance and direction to target

        val inPathFromSrcToCentre = src==mid | includingSelf.anyHood {
          nbr(parentToCentre) == mid
        } // am I in path from src to centre?
        val inPathFromTargetToCentre = dependentNodes.contains(target) // am I in path from target to centre?
        //val middle = anyHood(distToSource + nbrRange < nbr(distToSource)) // do I improve distance src-target?
        val inRegion = inPathFromSrcToCentre || inPathFromTargetToCentre // || middle

        val status: Status = branch(mid == target) {
          mux[Status](rep(0)(_+1)==1) { Output } { Terminated }
        } { if (inRegion) { Bubble } else { External } }

        env.put("dist_to_source", distToSource)
        env.put("dist_to_target", distToTarget)
        env.put("in_region", inRegion)
        env.put("in_path_from_source_to_center", inPathFromSrcToCentre)
        env.put("in_path_from_target_to_center", inPathFromTargetToCentre)
        env.put(s"proc_${src}_${target}", s"Msg '$msg'. Status: $status")

        (msg, status)
      }
    } }

    val (distToCentre, parentToCentre) = distanceToWithParent(centre == mid)

    val dependentNodes = rep(Set[ID]()){ case (s: Set[ID]) =>
      excludingSelf.unionHoodSet[ID](mux( nbr{parentToCentre}==mid ){ nbr(s) }{ Set[ID]() }) + mid
    } // set of nodes whose path towards gen passes through me

    env.put("gradient", distToCentre)
    env.put("parent_to_centre", parentToCentre)
    env.put("dependend_nodes", dependentNodes)

    val targets_found: Map[InitParams, Result] =
      sspawn[InitParams,RuntimeParams,Result](chatComputation,
        newTargets.map(t => (source, t, s"Msg from $mid to $t")),
        (distToCentre, parentToCentre, dependentNodes))
    targets_found
  }

//  implicit class RichFieldOps(fo: FieldOps) {
//    def everyHood(p: => Boolean): Boolean = {
//      fo.foldhoodTemplate(true)(_&&_)(nbr{p})
//    }
//  }

  /*****************************
  ** MAIN PROGRAM ENTRY POINT **
  ******************************/

  override def main() = {
    val newTargets: Set[ID] = branch(mid==source){
      val t = timer(200)
      if(t > 140 && t < 180) Set(targets(0))
      else Set(targets(1))
    }{ Set() }
    val chg = captureChange(newTargets)

    env.put("target", targets.contains(mid))
    env.put("newtargets", chg)
    env.put("source", source==mid)
    env.put("centre", centre==mid)

    env.put(SIM_METRIC_N_PROCS_RUN, 0.0)

    chat(centre, if(chg) newTargets else Set[ID]())
  }

  /*****************************
   ** Functions **
   ******************************/

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