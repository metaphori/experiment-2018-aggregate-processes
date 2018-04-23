package it.unibo.replicated

import java.util.Comparator

import it.unibo.alchemist.model.implementations.molecules.SimpleMolecule
import it.unibo.alchemist.model.interfaces.Node
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.{CustomSpawn, ScafiAlchemistSupport}

object Metrics {
}

class Gossip extends AggregateProgram
  with StandardSensors with ScafiAlchemistSupport with FieldUtils with CustomSpawn
  with BlockT with BlockG with BlockC with BlockS {
  override type MainResult = Any

  // FIX ISSUE IN SCAFI STDLIB
  override def randomUid: (Double, ID) = rep((thisRoundRandom), mid()) { v => (v._1, mid()) }
  def thisRoundRandom: Double = try {
    env.get[Double]("thisRoundRandom")
  } catch { case _ => env.put[Double]("thisRoundRandom",nextRandom); env.get[Double]("thisRoundRandom") }

  import Builtins.Bounded

  def gossipNaive[T:Bounded](value: T) = {
    rep(value)( max =>
      maxHood(nbr(max))
    )
  }

  def gossipGC[T](value: T)(implicit ev: Bounded[T]) = {
    val leader = S(grain = Double.PositiveInfinity, metric = nbrRange)
    env.put("leader", leader)
    broadcast(leader, C[Double,T](
      potential = distanceTo(leader),
      acc = ev.max(_,_),
      local = value,
      Null = ev.bottom))
  }

  def gossipReplicated[T](value: T, p: Double, k: Int)(implicit ev: Bounded[T]) = {
    (replicated{
      gossipGC[T]
    }(value,p,k) ++ Map[Long,T](Long.MaxValue -> value)).minBy[Long](_._1)._2
  }

  import it.unibo.Spawn._

  def replicated[T,R](proc: T => R)(argument: T, period: Double, numReplicates: Int) = {
    val lastPid = sharedTimerWithDecay[Double](period, dt(whenNan = 0)).toLong
    val newProcs = if(captureChange(lastPid)) Set(lastPid) else Set[Long]()
    val replicates = sspawn[Long,T,R]((pid: Long) => (arg) => {
      (proc(arg), if(lastPid - pid < numReplicates){ Output } else { External })
    }, newProcs, argument)

    env.put("clock", lastPid)
    env.put("dt", dt(whenNan = 0))
    env.put("replicates", replicates)

    replicates
  }

  val f: java.util.function.Function[_>:Node[Any],_<:Double] = _.getConcentration(new SimpleMolecule("sensor")).asInstanceOf[Double]
  def gossipOracle(): Double = environment.getNodes.stream().map[Double](f)
    .max((o1: Double, o2: Double) => o1.compareTo(o2)).get()

  def maxVal = env.get[Double]("maxsense")
  def senseValue = nextRandom*maxVal
  var sensedValue: Double = _

  override def main = {
    sensedValue = senseValue
    env.put[Double]("sensor", sensedValue)

    val gnaive = gossipNaive(sensedValue)
    val ggc = gossipGC(sensedValue)
    val grep = gossipReplicated(sensedValue, p = 30, k = 5)
    val gopt = gossipOracle()
    env.put("gossip_naive", Math.pow(gopt-gnaive,2))
    env.put("gossip_gc", Math.pow(gopt-ggc,2))
    env.put("gossip_repl", Math.pow(gopt-grep,2))
    env.put("gossip_opt", gopt)
    grep
  }

  /************ FUNCTIONS *************/

  def impulsesEvery[T : Numeric](d: T, dt: T): Boolean =
    rep(false){ impulse =>
      branch(impulse) { false } { T(d,dt)==0 }
    }

  def captureChange[T](x: T, initially: Boolean = true) = rep((Option.empty[T],false)) { case (value, _) =>
    (Some(x), value.map(_ != x).getOrElse(initially))
  }._2
}