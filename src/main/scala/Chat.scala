import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

class Chat extends AggregateProgram
  with StandardSensors with ScafiAlchemistSupport with FieldUtils with CustomSpawn with BlockT with BlockG with BlockC {
  override type MainResult = Any

  val centres = Set(189)
  val targets = Vector(200,77)
  val source = 0


  val startEvery = 20 // A generation impulse every 20 time units

  def isGenerator = centres.contains(mid)

  def delta: Int = dt(whenNan = 0).toInt


  def chat(centres: Set[ID], newTargets: Set[ID]) = {
    type InitParams = (ID, ID, String)  // source, target, msg
    type RuntimeParams = (Double, ID, Set[ID])  // dist to centre, parent to centre, set of nodes
    type Result = String

    val chatComputation: Proc[InitParams, RuntimeParams, Result] = {
      case (src: ID, target: ID, msg: String) => { case (distToCentre, parentToCentre, dependentNodes) => {
        val distToSource = distanceTo(src == mid)
        val (distToTarget, parentInPathToTarget) = distanceToWithParent(target == mid) // distance and direction to target
        val inPathFromSrcToCentre = includingSelf.anyHood {
          nbr(parentToCentre) == mid
        } // am I in path from src to gen?
        val inPathFromTargetToCentre = dependentNodes.contains(target) // am I in path from target to gen?
        //val middle = anyHood(distToSource + nbrRange < nbr(distToSource)) // do I improve distance src-target?
        val inRegion = inPathFromSrcToCentre || inPathFromTargetToCentre // || middle
        val finished = rep((false,false)){ case (startFinish, done) =>
          if(startFinish) env.put("finish", 1) else env.put("finish", 0)
          (mid==target | startFinish | excludingSelf.anyHood(nbr(startFinish)), includingSelf.everyHood(nbr(startFinish)))
        }._2
        val status: Status = if (inRegion && !finished) {
          if (mid == target) {
            env.put("bubble", 2)
            Output
          } else {
            env.put("bubble", 1)
            Bubble
          } // output is interested only in "src"
        } else {
          env.put("bubble", 0)
          External
        }
        (msg, status)
      }
    } }

    val (distToCentre, parentToCentre) = distanceToWithParent(centres.contains(mid))
    val dependentNodes = rep(Set[ID]()){ case (s: Set[ID]) =>
      excludingSelf.unionHoodSet[ID](mux( nbr{parentToCentre}==mid ){ nbr(s) }{ Set[ID]() }) + mid
    } // set of nodes whose path towards gen passes through me

    val targets_found: Map[InitParams, Result] =
      spawn[InitParams,RuntimeParams,Result](chatComputation,
        newTargets.map(t => (source, t, s"Msg from $mid to $t")),
        (distToCentre, parentToCentre, dependentNodes))
    targets_found
  }

  implicit class RichFieldOps(fo: FieldOps) {
    def everyHood(p: => Boolean): Boolean = {
      fo.foldhoodTemplate(true)(_&&_)(nbr{p})
    }
  }

  /*****************************
  ** MAIN PROGRAM ENTRY POINT **
  ******************************/

  override def main() = {
    val newTargets: Set[ID] = branch(mid==source){
      val t = timer(200)
      if(t > 150 && t < 180) Set(targets(0))
      else Set(targets(1))
    }{ Set() }
    val chg = captureChange(newTargets)

    env.put("target", targets.contains(mid))
    env.put("newtargets", chg)
    env.put("source", source==mid)
    env.put("centre", centres.contains(mid))

    chat(centres, if(chg) newTargets else Set[ID]())
  }

  /*****************************
   ** Functions **
   ******************************/

  def distanceToWithParent(source: Boolean): (Double, ID) = {
    rep((Double.PositiveInfinity, mid)){ case (dist, parent) =>
      excludingSelf.minHoodSelector(nbr{dist})(nbr{(dist,mid)}).getOrElse((Double.PositiveInfinity,mid))
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