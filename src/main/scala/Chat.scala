import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

class Chat extends AggregateProgram
  with StandardSensors with ScafiAlchemistSupport with FieldUtils with CustomSpawn with BlockT with BlockG with BlockC {
  override type MainResult = Any

  val generators = Set(189,285,294,125,114)
  val startEvery = 20 // A generation impulse every 20 time units

  def isGenerator = generators.contains(mid)

  def delta: Int = dt(whenNan = 0).toInt


  def chat(generators: Set[ID], newTargets: Set[ID]) = {
    val (distToGen, parentInPathToGen) = distanceToWithParent(generators.contains(mid))
    val dependentNodes = rep(Set[ID]()){ case (s: Set[ID]) =>
      excludingSelf.unionHoodSet[ID](mux( nbr{parentInPathToGen}==mid ){ nbr(s) }{ Set[ID]() }) + mid
    } // set of nodes whose path towards gen passes through me

    type InitParams = (ID, ID, String)  // source, target, msg
    type RuntimeParams = Unit             // nothing
    type Result = String

    val chatComputation: Proc[InitParams, RuntimeParams, Result] = {
      case (src: ID, target: ID, msg: String) => { case () => {
        val distToSource = distanceTo(src == mid)
        val (distToTarget, parentInPathToTarget) = distanceToWithParent(target == mid) // distance and direction to target
        val inPathFromSrcToGen = includingSelf.anyHood {
          nbr(parentInPathToGen) == mid
        } // am I in path from src to gen?
        val inPathFromTargetToGen = dependentNodes.contains(target) // am I in path from target to gen?
        //val middle = anyHood(distToSource + nbrRange < nbr(distToSource)) // do I improve distance src-target?
        val inRegion = inPathFromSrcToGen || inPathFromTargetToGen // || middle
        //val finished = gossipEver(old_targets.contains(src)) // if target gets removed, computation is over
        val status: Status = if (inRegion) {
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

    val targets_found: Map[InitParams, Result] =
      spawn[InitParams,RuntimeParams,Result](chatComputation,
        newTargets.map(t => (source, t, s"Msg from $mid to $t")).toList,
        Set())
    targets_found
  }

  /*****************************
  ** MAIN PROGRAM ENTRY POINT **
  ******************************/

  val source = 0
  override def main() = {
    val newTargets: Set[ID] = branch(mid==source){
      val t = timer(200)
      if(t > 150 && t < 180) Set(200)
      else Set(77)
    }{ Set() }
    val chg = captureChange(newTargets)

    env.put("target", Set(200,77).contains(mid))
    env.put("newtargets", chg)
    env.put("source", source==mid)
    env.put("generator", generators.contains(mid))

    chat(generators, if(chg) newTargets else Set[ID]())
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