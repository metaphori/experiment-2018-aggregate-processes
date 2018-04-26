package it.unibo.replicated

import java.util.Objects

import it.unibo.alchemist.model.implementations.actions.FireOnVesuvius
import it.unibo.alchemist.model.implementations.molecules.SimpleMolecule
import it.unibo.alchemist.model.interfaces.{Molecule, Node}
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.{CustomSpawn, ScafiAlchemistSupport}

object Metrics {
}

class Gossip extends AggregateProgram
  with StandardSensors with ScafiAlchemistSupport with FieldUtils with CustomSpawn
  with BlockT with BlockG with BlockC with BlockS {
  import Builtins._
  override type MainResult = Any

  // FIX ISSUE IN SCAFI STDLIB
  override def randomUid: (Double, ID) = rep[(Double, ID)]((nodeRandom, mid())) { v => (v._1, mid()) }
  def nodeRandom: Double = try {
    env.get[Double]("nodeRandom")
  } catch { case _ => env.put[Double]("nodeRandom",nextRandom); env.get[Double]("nodeRandom") }

  def gossipNaive[T](value: T)(implicit ev: Bounded[T]) = {
    Objects.requireNonNull(value)
    if (value.isInstanceOf[Double] && value.asInstanceOf[Double].isNaN) throw new IllegalStateException()
    rep(value)( max =>
      ev.max(value, maxHoodPlus(nbr(ev.max(max, value))))
    )
  }

  implicit val bottomDouble = new Bounded[Double] {
    def top: Double = Double.PositiveInfinity
    def bottom: Double = Double.NegativeInfinity // THIS FIX
    def compare(a: Double, b: Double): Int = (a-b).signum
  }

  def valueBroadcast[V: Bounded](source: Boolean, field: V): V =
    Gdef[V](source, field, v => v, nbrRange)

  def Gdef[V: Bounded](source: Boolean, field: V, acc: V => V, metric: => Double): V =
    rep((Double.PositiveInfinity, field)) { case (dist, value) =>
      mux(source) {
        (0.0, field)
      } {
        excludingSelf.minHoodLoc((Double.PositiveInfinity,field)) {
          (nbr { dist } + metric, acc(nbr { value }))
        }
      }
    }._2

  def gossipGC[T](value: T)(implicit ev: Bounded[T]) = {
    Objects.requireNonNull(value)
    if (value.isInstanceOf[Double] && value.asInstanceOf[Double].isNaN) throw new IllegalStateException()
    val leader = S(grain = Double.PositiveInfinity, metric = nbrRange)
    env.put("leader", leader)
    valueBroadcast(leader, C[Double,T](
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

  def square[T](x: T)(implicit ev: Numeric[T]): T = ev.times(x, x)

  override def main = {
    sensedValue = senseValue
    if(senseValue < 0 || senseValue > 1 || senseValue.isNaN) throw new IllegalStateException()
    val naive = gossipNaive(sensedValue)
    val scg = gossipGC(sensedValue)
    val replicated = gossipReplicated(sensedValue, p = env.get[Double]("p"), k = env.get[Int]("k"))
    val oracle = gossipOracle()
    import it.unibo.alchemist.model.interfaces.Time
    val trueOracle = FireOnVesuvius.truth(sense[Time]("time").toDouble)
    env.put("gossip_naive_val", naive)
    env.put("gossip_replicated_val", replicated)
    env.put("gossip_gc_val", scg)
    env.put("gossip_opt_val", oracle)
    env.put("gossip_true_val", trueOracle)
    env.put("gossip_naive_err", Math.pow(oracle-naive,2))
    env.put("gossip_replicated_err", Math.pow(oracle-replicated,2))
    env.put("gossip_gc_err", Math.pow(oracle-scg,2))
    env.put("gossip_naive_true_err", square(trueOracle-naive))
    env.put("gossip_gc_true_err", square(trueOracle-scg))
    env.put("gossip_replicated_true_err", square(trueOracle-replicated))
    env.put("oracle_true_err", square(oracle-trueOracle))
    replicated
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