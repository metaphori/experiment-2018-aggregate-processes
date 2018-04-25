package it.unibo.replicated

import it.unibo.alchemist.model.implementations.molecules.SimpleMolecule
import it.unibo.alchemist.model.interfaces.{Molecule, Node}
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.{CustomSpawn, ScafiAlchemistSupport}

object Metrics {
}

class Gossip extends AggregateProgram
  with StandardSensors with ScafiAlchemistSupport with FieldUtils with CustomSpawn
  with BlockT with BlockG with BlockC with BlockS {
  override type MainResult = Any

  // FIX ISSUE IN SCAFI STDLIB
  override def randomUid: (Double, ID) = rep((nodeRandom), mid()) { v => (v._1, mid()) }
  def nodeRandom: Double = try {
    env.get[Double]("nodeRandom")
  } catch { case _ => env.put[Double]("nodeRandom",nextRandom); env.get[Double]("nodeRandom") }


  import Builtins.Bounded

  def gossipNaive[T:Bounded](value: T) = {
    rep(value)( max =>
      maxHoodPlus(nbr(max))
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
      gossipNaive[T]
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

  import scala.collection.JavaConverters._
  implicit def strToMol(s: String): Molecule = new SimpleMolecule(s)
  def gossipOracle(): Double = environment.asScala.map (_.getConcentration("sensed").asInstanceOf[Double]).max

  def senseValue = env.get[Number]("sensed").doubleValue
  var sensedValue: Double = _

  override def main = {
    sensedValue = senseValue

    val gnaive = gossipNaive(sensedValue)
    val ggc = gossipGC(sensedValue)
    val grep = gossipReplicated(sensedValue, p = env.get[Double]("p"), k = env.get[Int]("k"))
    val gopt = gossipOracle()
    env.put("gossip_naive_val", gnaive)
    env.put("gossip_gc_val", ggc)
    env.put("gossip_repl_val", grep)
    env.put("gossip_opt_val", gopt)
    env.put("gossip_naive_err", Math.pow(gopt-gnaive,2))
    env.put("gossip_gc_err", Math.pow(gopt-ggc,2))
    env.put("gossip_repl_err", Math.pow(gopt-grep,2))
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